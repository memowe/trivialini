module Main where

import Trivialini
import Test.Hspec
import Data.Map
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

testIniParsing :: Spec
testIniParsing = describe "Ini parsing" $ do
  it "Complex ini data" $
    read exampleIni `shouldBe` expectedIni
  it "parse . show . parse = parse" $
    let intermediatini = read exampleIni :: Ini
    in  (read . show) intermediatini `shouldBe` expectedIni

testIniFileReading :: Spec
testIniFileReading = describe "Read ini file" $ do
  loadedIni <- runIO $ withSystemTempFile "trivialini-test.ini" $ \fp h -> do
    hPutStr h exampleIni >> hClose h
    readIniFile fp
  it "Expected complete ini data" $
    loadedIni `shouldBe` sections expectedIni

main :: IO ()
main = hspec $ describe "Unit tests" $ do
  testIniParsing
  testIniFileReading
