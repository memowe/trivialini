module Main where

import System.Exit  ( exitFailure )
import Trivialini   ( Config, Config(..), readConfig )
import Data.Map     ( fromList, (!) )

exampleIni =
    "foo = bar\n\
    \\n\
    \answer = 42\n\
    \[section name]\n\
    \not a key = nope\n\
    \answer = 17\n\
    \"

expectedConfig =
    Config "default" (fromList [("foo", "bar"), ("answer", "42")]) $
    Config "section name" (fromList [("answer", "17")]) Empty

parsedConfig = readConfig exampleIni

roundTripConfig = readConfig $ show parsedConfig

configParsingOK :: String -> Config -> Bool
configParsingOK ini expected =
        parsedConfig == expectedConfig
    &&  parsedConfig == roundTripConfig

main = if configParsingOK exampleIni expectedConfig
    then putStrLn "OK"
    else putStrLn "Nope" >> exitFailure
