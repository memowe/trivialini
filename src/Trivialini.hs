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
  ) where

import Trivialini.Ini ( Ini(..), showIni )

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

"Trivialini" parses this data as `Ini`, which is simply a 'Data.Map.Map' of
'Data.Map.Map's of 'String's:
-}

-- | Read 'Ini' data from a given filename
readIniFile :: FilePath -> IO Ini
readIniFile file = readIni <$> readFile file
