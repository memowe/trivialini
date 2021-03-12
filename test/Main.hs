module Main where

import System.Exit  ( exitFailure )
import Trivialini   ( Ini, Ini(..), iniSectionMap, iniValue, readIni )
import Data.Map     ( fromList, (!) )

exampleIni =
    "foo = bar\n\
    \\n\
    \answer    =42\n\
    \[section name]\n\
    \not a key = nope\n\
    \answer= 17\n\
    \"

expectedDefaultAnswer  = "42"
expectedDefaultSection = fromList [
        ("foo", "bar"),
        ("answer", expectedDefaultAnswer)
    ]
expectedIni =
    Ini "default" expectedDefaultSection $
    Ini "section name" (fromList [("answer", "17")]) Empty

parsedIni = readIni exampleIni

iniParsingOK :: Bool
iniParsingOK
    =   parsedIni == expectedIni
    &&  parsedIni == roundTripIni
    where roundTripIni = readIni $ show parsedIni

iniAccessOK :: Bool
iniAccessOK
    =   defaultMap      == expectedDefaultSection
    &&  defaultAnswer   == expectedDefaultAnswer
    where
        defaultMap      = iniSectionMap parsedIni "default"
        defaultAnswer   = iniValue parsedIni "default" "answer"

main = if iniParsingOK && iniAccessOK
    then putStrLn "OK"
    else putStrLn "Nope" >> exitFailure
