import Control.DeepSeq
import Control.Monad
import Data.Function
import Data.List
import Data.Int
import Data.Word
import Debug.Trace
import System.Environment

import Options.Applicative
import Data.Elf
import Data.Dwarf
import Data.Serialize.Get

import qualified Data.ByteString as B
import qualified Data.Map as M

sectionData :: String -> Elf -> B.ByteString
sectionData name elf =
  case find ((== name) . elfSectionName) (elfSections elf) of
    Just sect -> elfSectionData sect

stripPiece :: ElfData -> B.ByteString -> Maybe (B.ByteString, B.ByteString)
stripPiece endian debugInfo
  | B.null debugInfo = Nothing
  | otherwise = case runGet getLength debugInfo of
    Right length -> Just (B.splitAt length debugInfo)
    where getLength = do
            len <- w32
            case len of
              0xffffffff -> do
                realLen <- w64
                return $ fromIntegral (12 + realLen)
              _ | len < 0xffffff00 -> return $ fromIntegral (4 + len)
          (w32, w64) = case endian of
            ELFDATA2LSB -> (getWord32le, getWord64le)
            ELFDATA2MSB -> (getWord32be, getWord64be)

data Enumeration = Enumeration String [(String, Int64)] deriving (Show)

enumerations :: M.Map Word64 DIE -> [Enumeration]
enumerations dies = trace ("processing " ++ cu)
                    [ Enumeration n $!! vals die
                    | die <- M.elems dies, 
                      dieTag die == DW_TAG_enumeration_type, 
                      let n = name die,
                      n /= "<anonymous enum>"
                    ]
  where name die = case die !? DW_AT_name of [DW_ATVAL_STRING s] -> s
        vals die = [ (name child, case child !? DW_AT_const_value of
                         [DW_ATVAL_INT v] -> v)
                   | child <- map (dies M.!) (dieChildren die)
                   ]
        cu = name $ head (M.elems dies)

dedupeEnumeration :: Enumeration -> Enumeration
dedupeEnumeration (Enumeration name vals) = Enumeration name vals'
  where vals' = nubBy ((==) `on` snd) vals

options :: ParserInfo (Bool, String)
options = info (helper <*> args)
          (fullDesc <> progDesc "generate a C header file listing enums defined in FILE")
  where args = (,)
               <$> switch (long "dedupe" <>
                           help "drop enumeration values equal to previous ones")
               <*> argument str (metavar "FILE")

main = execParser options >>= \(dedupe, filename) -> do
  elf <- fmap parseElf (B.readFile filename)
  let debugInfo = sectionData ".debug_info" elf
      debugAbbrev = sectionData ".debug_abbrev" elf
      debugStr = sectionData ".debug_str" elf
      debugInfoPieces = unfoldr (stripPiece (elfData elf)) debugInfo
      parseDwarf piece = parseDwarfInfo
                         (elfData elf == ELFDATA2LSB)
                         piece debugAbbrev debugStr
  let enums = foldl' (\es (Enumeration k v) ->
                       M.insertWith'
                       (\new old ->
                         if new /= old
                         then error ("Conflict: " ++ show k ++ ": " ++
                                     show new ++ " /= " ++ show old)
                         else new)
                       k v es
                     ) M.empty $
              concatMap (maybeDedupe . enumerations . parseDwarf) debugInfoPieces
                where maybeDedupe | dedupe = map dedupeEnumeration
                                  | otherwise = id
  forM_ (M.assocs enums) $ \(name, vals) -> do
    putStrLn $ "enum " ++ name ++ " {"
    forM_ vals $ \(valname, valval) -> do
      putStrLn $ "        " ++ valname ++ " = " ++ show valval ++ ","
    putStrLn "};"
    putStrLn ""
