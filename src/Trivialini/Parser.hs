{-|
Module      : Trivialini.Parser
Description : Parsing of Ini strings
Copyright   : (c) 2021 Mirko Westermeier
License     : MIT

Parsing of 'Ini' strings
-}

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

without :: String -> ReadP String
without str = munch1 (`notElem` str)

newlines :: ReadP ()
newlines = skipMany1 (char '\n')

sectionName :: ReadP String
sectionName = do
  name <- trim <$> between (char '[') (char ']') (without "=\n]")
  newlines
  return name

pair :: ReadP (String, String)
pair = do
  key <- trim <$> without "\n[="
  char '='
  value <- trim <$> without "\n"
  newlines
  return (key, value)

section :: ReadP (String, Map String String)
section = do
  name  <- sectionName
  pairs <- many pair
  return (name, fromList pairs)

sections :: ReadP Ini
sections = do
  secs <- many section
  return $ fromList secs

-- | Read 'Ini' data from a given 'String' in ini format.
readIni :: String -> Ini
readIni = fst . last . readP_to_S sections

-- | The 'ReadS' parser of 'Ini' data, useful for debugging.
parseIni :: ReadS Ini
parseIni = readP_to_S sections
