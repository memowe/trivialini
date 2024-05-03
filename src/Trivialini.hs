{-|
Module      : Trivialini
Description : Ultra light weight ini file parser
Copyright   : (c) 2021 Mirko Westermeier
License     : MIT

Ultra light weight ini file parser
-}

module Trivialini
  (
  -- * Ini files and data
  -- $intro
    readIniFile
  , readIniFileStrings
  -- * Ini data is a Map of Maps
  , Ini(..)
  -- * Simple 'String' 'Map' conversion
  , toStringMap
  ) where

import Trivialini.SafeTypes
import Data.Map (Map, fromList, assocs, mapKeys)
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP
import Control.Monad

{- $intro
Consider a simple ini file @config.ini@ like this:

@
[something]
foo = bar

[something else]
answer = 42
name = Boaty McBoatface
@

There are two /sections/ (inbetween @"["@ and @"]"@) defined, @"something"@
and @"something else"@. These sections contain a dictionary of strings each,
the keys being some string followed by @"="@, and anything else until end of
the line as values. The leading and trailing spaces in section headers, keys
and values are trimmed.
-}

-- | Read t'Ini' data from a given filename
readIniFile :: FilePath -> IO Ini
readIniFile = fmap read . readFile

-- | Like 'readIniFile', but results in a stringified nested map
readIniFileStrings :: FilePath -> IO (Map String (Map String String))
readIniFileStrings = fmap (toStringMap . read) . readFile

-- | As ini files consist of sections with a name, each with a list of
-- key-value pairs, A "two-dimensional" 'Map' of 'String's seems to be very
-- natural. However, since the formatting of ini files doesn't allow arbitrary
-- arbitrary characters, restricted types are used here, that are thin wrappers
-- around 'String's:
newtype Ini = Ini { sections :: Map IniHeading (Map IniKey IniValue) }
  deriving
    ( Eq -- ^ Default Eq instance
    )

-- | Stringification of t'Ini' data. The result can be parsed again as t'Ini'
-- data.
instance Show Ini where
  show = unlines . map section . assocs . sections
    where section (name, sec) = "[" ++ getHeading name ++ "]\n" ++ pairs sec
          pairs               = unlines . map pair . assocs
          pair (k, v)         = getKey k ++ " = " ++ getValue v

-- | Parsing of Ini strings.
instance Read Ini where
  readsPrec _ = readP_to_S parser
    where parser  = Ini . fromList <$> many section
          section = do  name <- trim <$> between (char '[') (char ']' >> nls) (no "=\n]")
                        guard $ isValidHeading name
                        pairs <- many pair
                        return (fromJust (mkHdg name), fromList pairs)
          pair    = do  key <- trim <$> no "\n[="
                        val <- trim <$> between (char '=') nls (no "\n")
                        guard $ isValidKey key && isValidValue val
                        return (fromJust (mkKey key), fromJust (mkVal val))
          nls     = munch1 (=='\n')
          no      = munch1 . flip notElem
          trim    = dropWhile (==' ') . dropWhileEnd (==' ')

-- | Convert ini data (with restricted types) to nested 'String' 'Map's.
toStringMap :: Ini -> Map String (Map String String)
toStringMap = mapKeys getHeading . fmap sectionToStringMap . sections
  where sectionToStringMap = mapKeys getKey . fmap getValue
