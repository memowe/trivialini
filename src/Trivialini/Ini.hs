{-|
Module      : Trivialini.Ini
Description : The Ini data type
Copyright   : (c) 2021 Mirko Westermeier
License     : MIT

The Ini data type
-}

module Trivialini.Ini
    (
    -- * Ini data is a Map of Maps
      Ini(..)
    , showIni ) where

import Data.Map ( assocs, Map )

-- | As ini files consist of sections with a name, each with a list of
-- key-value pairs, A "two-dimensional" 'Map' of 'String's seems to be very
-- natural.
type Ini = Map String (Map String String)

-- | Stringification of 'Ini' data. The result can be parsed again as 'Ini'
-- data.
showIni :: Ini -> String
showIni = unlines . map section . assocs
    where
        section (name, sec) = "[" ++ name ++ "]\n" ++ pairs sec
        pairs               = unlines . map pair . assocs
        pair (k, v)         = k ++ " = " ++ v
