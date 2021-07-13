module Main where

import System.Exit  ( exitFailure )
import Trivialini   ( showIni, readIni )
import Data.Map     ( fromList )

exampleIni =
    "[xnorfzt]\n\
    \foo = bar\n\
    \\n\
    \answer    =42\n\
    \[section name]\n\
    \baz    =      quux\n\
    \not a key = nope\n\
    \ignored= 17\n\
    \"

expectedIni = fromList [
        ("xnorfzt", fromList [("foo", "bar"), ("answer", "42")]),
        ("section name", fromList [("baz", "quux")])
    ]

iniParsingOK :: Bool
iniParsingOK
    =   parsedIni == expectedIni
    &&  parsedIni == roundTripIni
    where
        parsedIni       = readIni exampleIni
        roundTripIni    = (readIni . showIni) parsedIni

main = if iniParsingOK
    then putStrLn "OK"
    else putStrLn "Nope" >> exitFailure
