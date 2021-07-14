-- | The parser of ini files
module Trivialini.Parser ( readIni, parseIni ) where

import Trivialini.Ini ( Ini )
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

-- | Ini string parser, useful for debugging
parseIni :: ReadS Ini
parseIni = readP_to_S sections
