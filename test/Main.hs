module Main where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Trivialini
import Data.Map (empty, elems, fromList)
import System.IO.Temp
import System.IO

exampleIni :: String
exampleIni =  "[xnorfzt]\n\
              \foo = bar\n\
              \\n\
              \x=17\n\
              \answer    =42\n\
              \[section name]\n\
              \ baz quux   =      quuux\n\
              \"

expectedIni :: Ini
expectedIni = Ini $ fromList [
    ("xnorfzt", fromList [("foo", "bar"), ("x", "17"), ("answer", "42")]),
    ("section name", fromList [("baz quux", "quuux")])
  ]

testIniParsingExample :: Spec
testIniParsingExample = describe "Example data parsing" $ do
  it "Correct INI data" $
    read exampleIni `shouldBe` expectedIni
  it "read . show changes nothing" $
    let intermediatini = read exampleIni :: Ini
    in  (read . show) intermediatini `shouldBe` expectedIni

testIniParsingArbitrary :: Spec
testIniParsingArbitrary = describe "Arbitrary data parsing" $ do
  prop "read . show changes nothing" $ \iniMap ->
    (iniMap /= empty && (empty `notElem` elems iniMap)) ==>
      let ini = Ini iniMap
      in  (sections . read . show) ini `shouldBe` iniMap

testIniFileReading :: Spec
testIniFileReading = describe "Read ini file" $ do
  loadedIni <- runIO $ withSystemTempFile "trivialini-test.ini" $ \fp h -> do
    hPutStr h exampleIni >> hClose h
    readIniFile fp
  it "Correct example INI data" $
    loadedIni `shouldBe` sections expectedIni

main :: IO ()
main = hspec $ describe "Ini parsing tests" $ do
  testIniParsingExample
  testIniParsingArbitrary
  testIniFileReading
