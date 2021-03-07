-- | Ultra light weight ini file parser
module Trivialini (config, configFile) where

import Data.Map         (Map, keys, empty, insert, (!))
import Text.Regex.TDFA  ((=~))

--
--  Data structure and accessors
--

-- data structure for "ini" file representation
data Config = Empty | Config String (Map String String) Config

instance Show Config where
    show Empty = ""
    show (Config sec defs rest) = secLn ++ defLns ++ "\n" ++ show rest
        where
            secLn   = "[" ++ sec ++ "]\n"
            defLns  = concatMap def $ keys defs
            def key = key ++ " = " ++ defs ! key ++ "\n"

-- Extract the list of all section names
sections :: Config -> [String]
sections Empty                  = []
sections (Config name _ rest)   = name : sections rest

-- Extract the map of config values for a given section name
section :: Config -> String -> Map String String
section Empty _                         = empty
section (Config name map rest) query
    | name == query                     = map
    | otherwise                         = section rest query

-- Extract a configuration data value for a given section and key
value :: Config -> String -> String -> String
value conf sec key = s ! key
    where s = section conf sec

--
--  Parsing data structure from text file
--

readConfig :: String -> Config
-- Preparation of config parsing
readConfig = simplify . rc "default" empty . lines

-- Remove sections without definitions
simplify :: Config -> Config
simplify Empty = Empty
simplify config@(Config sec defs rest)
    | null defs = rest
    | otherwise = config

rc :: String -> Map String String -> [String] -> Config
-- Build config data line by line (first argument is the current section)
rc sec values [] = Config sec values Empty
rc sec values (line:lines)
    | isSec     = Config sec values $ rc getSec empty lines
    | isKV      = rc sec insKV lines
    | otherwise = rc sec values lines -- No match: ignore that line
    where
        rxSec   = "^\\[(.+)\\]"
        rxKV    = "^([^ ]+) += +(.*)$"
        isSec   = line =~ rxSec :: Bool
        isKV    = line =~ rxKV  :: Bool
        getSec  = let [[_, sec]] = line =~ rxSec :: [[String]] in sec
        getKV   = let [[_, k, v]] = line =~ rxKV :: [[String]] in (k, v)
        insKV   = let (k, v) = getKV in insert k v values

--
--  Public interface
--

config :: String -> String -> String -> String
-- ^Return the config value for a given section and key from the given config
config = value . readConfig

configFile :: String -> String -> String -> IO String
-- ^Return the config file value for a given section and key
configFile file sec key = do
    content <- readFile file
    return $ config content sec key
