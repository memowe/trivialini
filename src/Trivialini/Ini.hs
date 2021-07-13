-- | Trivialini data type and utilities
module Trivialini.Ini ( Ini(..), showIni ) where

import Data.Map ( assocs, Map )

-- | data structure for "ini" file representation
type Ini = Map String (Map String String)

_showMap :: Map String String -> String
_showMap = unlines . map line . assocs
    where line (k, v) = k ++ " = " ++ v

-- | Nice string representation of Ini data.
--   The output parses as Ini data.
showIni :: Ini -> String
showIni = unlines . map section . assocs
    where section (name, sec) = "[" ++ name ++ "]\n" ++ _showMap sec
