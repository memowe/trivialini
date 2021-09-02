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
  ) where

import Data.Map ( assocs, Map )

-- | As ini files consist of sections with a name, each with a list of
-- key-value pairs, A "two-dimensional" 'Map' of 'String's seems to be very
-- natural.
newtype Ini = Ini { pairMap :: Map String (Map String String) }
  deriving
    ( Eq -- ^ Default Eq instance
    )

-- | Stringification of 'Ini' data. The result can be parsed again as 'Ini'
-- data.
instance Show Ini where
  show = unlines . map section . assocs . pairMap
    where section (name, sec) = "[" ++ name ++ "]\n" ++ pairs sec
          pairs               = unlines . map pair . assocs
          pair (k, v)         = k ++ " = " ++ show v
