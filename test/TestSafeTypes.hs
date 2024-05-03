{-# OPTIONS_GHC -Wno-orphans #-}
module TestSafeTypes where

import Trivialini
import Trivialini.SafeTypes
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Data.String
import Control.Applicative
import Control.Exception

instance Arbitrary IniHeading where
  arbitrary = arbitrary `suchThatMap` mkHdg

instance Arbitrary IniKey where
  arbitrary = arbitrary `suchThatMap` mkKey

instance Arbitrary IniValue where
  arbitrary = arbitrary `suchThatMap` mkVal

instance Arbitrary Ini where
  arbitrary = Ini <$> arbitrary

testArbinitrary :: Spec
testArbinitrary = describe "Safe types tests" $ do
  modifyMaxDiscardRatio (const 1000) $ do -- Neccessary, but tests are fast

    context "Ini headings" $ do

      it "Correct invalid characters" $
        invalidHdgChars `shouldBe` "=]\n"

      describe "Validity predicates" $ do
        prop "valid" $ \s -> safeHdg s ==>
          isValidHeading s `shouldBe` True
        prop "invalid" $ \s -> not (safeHdg s) ==>
          isValidHeading s `shouldBe` False

      describe "Data construction" $ do
        prop "valid" $ \s -> safeHdg s ==>
          getHeading <$> mkHdg s `shouldBe` Just s
        prop "invalid" $ \s -> not (safeHdg s) ==>
          mkHdg s `shouldBe` Nothing

      prop "Show instance" $ \s -> safeHdg s ==>
        show <$> mkHdg s `shouldBe` Just s

      describe "IsString instance" $ do
        prop "valid" $ \s -> safeHdg s ==>
          getHeading (fromString s) `shouldBe` s
        prop "invalid" $ \s -> not (safeHdg s) ==>
          evaluate (fromString s :: IniHeading)
            `shouldThrow` errorCall ("Not a heading: " ++ s)

    context "Ini keys" $ do

      it "Correct invalid characters" $
        invalidKeyChars `shouldBe` "=[\n"

      describe "Validity predicates" $ do
        prop "valid" $ \s -> safeKey s ==>
          isValidKey s `shouldBe` True
        prop "invalid" $ \s -> not (safeKey s) ==>
          isValidKey s `shouldBe` False

      describe "Data construction" $ do
        prop "valid" $ \s -> safeKey s ==>
          getKey <$> mkKey s `shouldBe` Just s
        prop "invalid" $ \s -> not (safeKey s) ==>
          mkKey s `shouldBe` Nothing

      prop "Show instance" $ \s -> safeKey s ==>
        show <$> mkKey s `shouldBe` Just s

      describe "IsString instance" $ do
        prop "valid" $ \s -> safeKey s ==>
          getKey (fromString s) `shouldBe` s
        prop "invalid" $ \s -> not (safeKey s) ==>
          evaluate (fromString s :: IniKey)
            `shouldThrow` errorCall ("Not a key: " ++ s)

    context "Ini values" $ do

      it "Correct invalid characters" $
        invalidValChars `shouldBe` "\n"

      describe "Validity predicates" $ do
        prop "valid" $ \s -> safeVal s ==>
          isValidValue s `shouldBe` True
        prop "invalid" $ \s -> not (safeVal s) ==>
          isValidValue s `shouldBe` False

      describe "Data construction" $ do
        prop "valid" $ \s -> safeVal s ==>
          getValue <$> mkVal s `shouldBe` Just s
        prop "invalid" $ \s -> not (safeVal s) ==>
          mkVal s `shouldBe` Nothing

      prop "Show instance" $ \s -> safeVal s ==>
        show <$> mkVal s `shouldBe` Just s

      describe "IsString instance" $ do
        prop "valid" $ \s -> safeVal s ==>
          getValue (fromString s) `shouldBe` s
        prop "invalid" $ \s -> not (safeVal s) ==>
          evaluate (fromString s :: IniValue)
            `shouldThrow` errorCall ("Not a value: " ++ s)

  where safeHdg = all (`notElem` invalidHdgChars) &&& isValidStr
        safeKey = all (`notElem` invalidKeyChars) &&& isValidStr
        safeVal = all (`notElem` invalidValChars) &&& isValidStr
        (&&&)   = liftA2 (&&)
