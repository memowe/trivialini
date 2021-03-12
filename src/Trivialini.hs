-- | Ultra light weight ini file parser
module Trivialini (
        Ini(..), iniSectionMap, iniValue,
        readIni, readIniFile,
        iniFileValue
    ) where

import Data.Map         (Map, keys, empty, insert, (!))
import Text.Regex.TDFA  ((=~))

--
-- Data structure and accessors
--

-- | data structure for "ini" file representation
data Ini = Empty | Ini String (Map String String) Ini
    deriving (Eq)

instance Show Ini where
    show Empty = ""
    show (Ini sec defs rest) = secLn ++ defLns ++ "\n" ++ show rest
        where
            secLn   = "[" ++ sec ++ "]\n"
            defLns  = concatMap def $ keys defs
            def key = key ++ " = " ++ defs ! key ++ "\n"

-- | Extract the map of ini values for a given section name
iniSectionMap :: Ini -> String -> Map String String
iniSectionMap Empty _  = empty
iniSectionMap (Ini name map rest) query
    | name == query = map
    | otherwise     = iniSectionMap rest query

-- | Extract an ini data value for a given section and key
iniValue :: Ini -> String -> String -> String
iniValue ini sec key = iniSectionMap ini sec ! key

--
-- Parsing Ini data structures
--

-- | Parse an ini string
readIni :: String -> Ini
readIni = _simplify . _rc "default" empty . lines

-- Helper: build ini data line by line (first argument: current section)
_rc :: String -> Map String String -> [String] -> Ini
_rc sec values [] = Ini sec values Empty
_rc sec values (line:lines)
    | isSec     = Ini sec values $ _rc getSec empty lines
    | isKV      = _rc sec insKV lines
    | otherwise = _rc sec values lines -- No match: ignore that line
    where
        rxSec   = "^\\[(.+)\\]"
        rxKV    = "^([^ ]+) += +(.*)$"
        isSec   = line =~ rxSec :: Bool
        isKV    = line =~ rxKV  :: Bool
        getSec  = let [[_, sec]] = line =~ rxSec :: [[String]] in sec
        getKV   = let [[_, k, v]] = line =~ rxKV :: [[String]] in (k, v)
        insKV   = let (k, v) = getKV in insert k v values

-- Helper: remove sections without definitions
_simplify :: Ini -> Ini
_simplify Empty = Empty
_simplify ini@(Ini sec defs rest)
    | null defs = rest
    | otherwise = ini

--
--  Convenience file system access
--

-- | Return the complete ini data from the given filename
readIniFile :: String -> IO Ini
readIniFile file = readIni <$> readFile file

-- | Return the ini file value for a given filename, section and key
iniFileValue :: String -> String -> String -> IO String
iniFileValue file sec key = do
    content <- readFile file
    return $ iniValue (readIni content) sec key
