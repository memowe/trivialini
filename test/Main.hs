module Main where

import Test.Tasty         ( defaultMain, testGroup, TestTree, withResource )
import Test.Tasty.HUnit   ( testCase, (@?=) )
import Trivialini         ( Ini(..), IniMap, readIniFile )
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

expectedIni = Ini $ fromList [
    ("xnorfzt", fromList [("foo", "bar"), ("x", "17"), ("answer", "42")]),
    ("section name", fromList [("baz quux", "quuux")])
  ]

testIniParsing = testGroup "Ini parsing"
  [ testCase "Complex ini data" $
      read exampleIni @?= expectedIni
  , testCase "parse . show . parse = parse" $
      let intermediatini  = read exampleIni :: Ini
      in  (read . show) intermediatini @?= expectedIni
  ]

testIniIO :: IO (FilePath, IniMap) -> TestTree
testIniIO ioData = testGroup "Read ini file"
  [ testCase "Expected complete ini data" $ do
      iniMap <- snd <$> ioData
      iniMap @?= sections expectedIni
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
