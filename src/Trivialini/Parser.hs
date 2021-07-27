-- | The parser of ini files
module Trivialini.Parser ( readIni, parseIni ) where

import Trivialini.Ini ( Ini )
import Data.Char ( isSpace )
import Data.List ( dropWhileEnd )
import Data.Map ( fromList, Map )
import Text.ParserCombinators.ReadP
    ( between, char, many, munch, munch1, readP_to_S,
      satisfy, skipMany, skipMany1, ReadP )

trim :: String -> String
trim = dropWhile (==' ') . dropWhileEnd (==' ')

newlines :: ReadP ()
newlines = skipMany1 (char '\n')

sectionName :: ReadP String
sectionName = do
    name <- trim <$> between (char '[') (char ']') (munch1 (`notElem` "=\n]"))
    newlines
    return name

pair :: ReadP (String, String)
pair = do
    key <- trim <$> munch1 (`notElem` "\n[=")
    char '='
    value <- trim <$> munch1 (/= '\n')
    newlines
    return (key, value)

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

-- | Ini string parser, useful for debugging
parseIni :: ReadS Ini
parseIni = readP_to_S sections
