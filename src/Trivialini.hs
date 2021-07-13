-- | Ultra light weight ini file parser
module Trivialini ( Ini(..), showIni, readIni, readIniFile ) where

import Data.Map ( assocs, fromList, Map )
import Text.ParserCombinators.ReadP
    ( between, char, many, munch1, readP_to_S,
      satisfy, skipMany1, skipSpaces, ReadP )

--
-- Data structure and accessors
--

-- | data structure for "ini" file representation
type Ini = Map String (Map String String)

_showMap :: Map String String -> String
_showMap = unlines . map line . assocs
    where line (k, v) = k ++ " = " ++ v

showIni :: Ini -> String
showIni = unlines . map section . assocs
    where section (name, sec) = "[" ++ name ++ "]\n" ++ _showMap sec

--
-- Parsing Ini data structures
--

sectionName :: ReadP String
sectionName = do
    name <- between (char '[') (char ']') (munch1 (/= ']'))
    skipMany1 (char '\n')
    return name

pair :: ReadP (String, String)
pair = do
    keyHead <- satisfy (`notElem` "\n =[")
    keyRest <- munch1 (`notElem` "\n =")
    skipSpaces
    char '='
    skipSpaces
    value <- munch1 (/= '\n')
    skipMany1 (char '\n')
    return (keyHead:keyRest, value)

section :: ReadP (String, Map String String)
section = do
    name <- sectionName
    pairs <- many pair
    return (name, fromList pairs)

sections :: ReadP Ini
sections = do
    secs <- many section
    return $ fromList secs

-- | Parse an ini string
readIni :: String -> Ini
readIni = fst . last . readP_to_S sections

--
--  Convenience file system access
--

-- | Return the complete ini data from the given filename
readIniFile :: String -> IO Ini
readIniFile file = readIni <$> readFile file
