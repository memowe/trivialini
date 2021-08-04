module Main where

import Test.Tasty           ( defaultMain, testGroup )
import Test.Tasty.HUnit     ( testCase, (@?=) )
import Trivialini.Ini       ( showIni )
import Trivialini.Parser    ( readIni )
import Data.Map             ( fromList )

exampleIni =
  "[xnorfzt]\n\
  \foo = bar\n\
  \\n\
  \x=17\n\
  \answer    =42\n\
  \[section name]\n\
  \ baz quux   =      quuux\n\
  \"

expectedIni = fromList [
    ("xnorfzt", fromList [("foo", "bar"), ("x", "17"), ("answer", "42")]),
    ("section name", fromList [("baz quux", "quuux")])
  ]

testIniParsing = testGroup "Ini parsing"
  [ testCase "Complex ini file" $
      readIni exampleIni @?= expectedIni
  , testCase "parse . show . parse = parse" $
      (readIni . showIni . readIni) exampleIni @?= expectedIni
  ]

main = defaultMain $ testGroup "Unit tests" [testIniParsing]
