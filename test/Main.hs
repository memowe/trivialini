module Main where

import System.Exit          ( exitFailure )
import Trivialini.Ini       ( showIni )
import Trivialini.Parser    ( readIni )
import Data.Map             ( fromList )

exampleIni =
    "[xnorfzt]\n\
    \foo = bar\n\
    \\n\
    \answer    =42\n\
    \[section name]\n\
    \baz    =      quux\n\
    \ is a valid key = yes\n\
    \seventeen= 17\n\
    \"

expectedIni = fromList [
        ("xnorfzt", fromList [("foo", "bar"), ("answer", "42")]),
        ("section name", fromList [
            ("baz", "quux"), ("is a valid key", "yes"), ("seventeen", "17")
        ])
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
