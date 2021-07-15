-- | The parser of ini files
module Trivialini.Parser ( readIni, parseIni ) where

import Trivialini.Ini ( Ini )
import Data.Char ( isSpace )
import Data.Map ( fromList, Map )
import Text.ParserCombinators.ReadP
    ( between, char, many, munch1, readP_to_S,
      satisfy, skipMany1, skipSpaces, ReadP )

sectionName :: ReadP String
sectionName = do
    name <- between (char '[') (char ']') (munch1 (/= ']'))
    skipMany1 (char '\n')
    return name

pair :: ReadP (String, String)
pair = do
    skipSpaces
    keyHead <- satisfy (`notElem` "\n =[")
    keyRest <- removeTrailingSpaces <$> munch1 (`notElem` "\n=")
    char '='
    skipSpaces
    value <- munch1 (/= '\n')
    skipMany1 (char '\n')
    return (keyHead:keyRest, value)
  where
    removeTrailingSpaces :: String -> String
    removeTrailingSpaces = reverse . removeLeadingSpaces . reverse
      where
        removeLeadingSpaces [] = []
        removeLeadingSpaces str@(x : xs) | isSpace x = removeLeadingSpaces xs
                                         | otherwise = str

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
