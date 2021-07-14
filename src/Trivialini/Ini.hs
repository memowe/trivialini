-- | Trivialini data type and utilities
module Trivialini.Ini ( Ini(..), showIni ) where

import Data.Map ( assocs, Map )

-- | data structure for "ini" file representation
type Ini = Map String (Map String String)

-- | Nice string representation of Ini data.
--   The output parses as Ini data.
showIni :: Ini -> String
showIni = unlines . map section . assocs
    where
        section (name, sec) = "[" ++ name ++ "]\n" ++ pairs sec
        pairs               = unlines . map pair . assocs
        pair (k, v)         = k ++ " = " ++ v
