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
  -- * Ini data is a Map of Maps
  , IniMap
  , Ini(..)
  ) where

import Data.Map (Map)
import qualified  Data.Map as M
import Data.List
import Text.ParserCombinators.ReadP

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
readIniFile :: FilePath -> IO IniMap
readIniFile file = sections . read <$> readFile file

-- | As ini files consist of sections with a name, each with a list of
-- key-value pairs, A "two-dimensional" 'Map' of 'String's seems to be very
-- natural.
type IniMap = Map String (Map String String)

-- | A wrapper type around an 'IniMap' with 'Show' and 'Read' instances.
newtype Ini = Ini { sections :: IniMap }
  deriving
    ( Eq -- ^ Default Eq instance
    )

-- | Stringification of t'Ini' data. The result can be parsed again as t'Ini'
-- data.
instance Show Ini where
  show = unlines . map section . M.assocs . sections
    where section (name, sec) = "[" ++ name ++ "]\n" ++ pairs sec
          pairs               = unlines . map pair . M.assocs
          pair (k, v)         = k ++ " = " ++ v

-- | Parsing of Ini strings.
instance Read Ini where
  readsPrec _ = readP_to_S parser
    where parser  = Ini . M.fromList <$> many section
          section = do  name  <- trim <$> between (char '[') (char ']' >> nls) (no "=\n]")
                        pairs <- many pair
                        return (name, M.fromList pairs)
          pair    = do  key <- trim <$> no "\n[="
                        val <- trim <$> between (char '=') nls (no "\n")
                        return (key, val)
          nls     = munch1 (=='\n')
          no      = munch1 . flip notElem
          trim    = dropWhile (==' ') . dropWhileEnd (==' ')
