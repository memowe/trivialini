module Main where

import Test.Tasty         ( defaultMain, testGroup, TestTree, withResource )
import Test.Tasty.HUnit   ( testCase, (@?=) )
import Trivialini         ( readIniFile )
import Trivialini.Ini     ( Ini(..), showIni )
import Data.Map           ( fromList )
import System.FilePath    ( (</>) )
import System.Directory   ( getTemporaryDirectory, removeFile )

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
  [ testCase "Complex ini data" $
      readIni exampleIni @?= expectedIni
  , testCase "parse . show . parse = parse" $
      (readIni . showIni . readIni) exampleIni @?= expectedIni
  ]

testIniIO :: IO (FilePath, Ini) -> TestTree
testIniIO ioData = testGroup "Read ini file"
  [ testCase "Expected complete ini data" $ do
      ini <- snd <$> ioData
      ini @?= expectedIni
  ]

testIniFileReading = withResource io cleanup testIniIO
  where
    io = do
      name  <- write
      ini   <- readIniFile name
      return (name, ini)
    write = do
      name <- (</> "trivialini-test.ini") <$> getTemporaryDirectory
      writeFile name exampleIni
      return name
    cleanup = removeFile . fst

main = defaultMain $ testGroup "Unit tests"
  [ testIniParsing
  , testIniFileReading
  ]
