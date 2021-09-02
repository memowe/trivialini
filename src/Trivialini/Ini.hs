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

import Data.Map ( assocs, Map, fromList )
import Data.List ( dropWhileEnd )
import Text.ParserCombinators.ReadP
    ( between, char, many, munch1, readP_to_S, skipMany1 )

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

-- | Parsing of Ini strings.
instance Read Ini where
  readsPrec _ = readP_to_S parser
    where parser  = Ini . fromList <$> many section
          section = do
            name  <- trim <$> between (char '[') (char ']' >> nls) (no "=\n]")
            pairs <- many pair
            return (name, fromList pairs)
          pair    = do
            key <- trim <$> no "\n[="
            val <- trim <$> between (char '=') nls (no "\n")
            return (key, val)
          nls     = skipMany1 (char '\n')
          no      = munch1 . flip notElem
          trim    = dropWhile (==' ') . dropWhileEnd (==' ')
