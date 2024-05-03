{- |
Ini data essentially consists of 'String's, that cannot contain every character
because of the simple format it is contained in. The types in this module are
restricted to contain only allowed characters. They also can not be empty or
start or end with whitespace. Values of these types can not be created with
data constructors. Use the @mk*@ functions instead!
-}

module Trivialini.SafeTypes
  (
    -- * Safe 'String' types
    IniHeading(getHeading), IniKey(getKey), IniValue(getValue)
      -- (No data constructors!)
    -- ** Value creation
  , mkHdg, mkKey, mkVal
    -- ** Validity predicates
  , isValidHeading, isValidKey, isValidValue
    -- ** Invalid character lists (useful for parsers)
  , invalidHdgChars, invalidKeyChars, invalidValChars
    -- ** Utility predicate
  , isValidStr
  ) where

import Data.Bool
import Data.Char
import Data.String
import Data.Maybe
import Control.Applicative

-- Utility function
guarded :: Alternative m => (a -> Bool) -> a -> m a
guarded = liftA2 (bool empty) pure

-- | A section heading
newtype IniHeading  = Hdg { getHeading  :: String } deriving (Eq, Ord)
-- | A key of a key-value pair
newtype IniKey      = Key { getKey      :: String } deriving (Eq, Ord)
-- | A value of a key-value pair
newtype IniValue    = Val { getValue    :: String } deriving (Eq, Ord)

invalidHdgChars :: String
invalidKeyChars :: String
invalidValChars :: String
invalidHdgChars = "=]\n"
invalidKeyChars = "=[\n"
invalidValChars = "\n"

isValidHeading  :: String -> Bool
isValidKey      :: String -> Bool
isValidValue    :: String -> Bool
isValidHeading  = all (`notElem` invalidHdgChars) &&& isValidStr
isValidKey      = all (`notElem` invalidKeyChars) &&& isValidStr
isValidValue    = all (`notElem` invalidValChars) &&& isValidStr

isValidStr :: String -> Bool
isValidStr =  (not . null)
          &&& (not . any isControl)
          &&& (not . isSpace . head)
          &&& (not . isSpace . last)

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = liftA2 (&&)

mkHdg :: String -> Maybe IniHeading
mkKey :: String -> Maybe IniKey
mkVal :: String -> Maybe IniValue
mkHdg = fmap Hdg . guarded isValidHeading
mkKey = fmap Key . guarded isValidKey
mkVal = fmap Val . guarded isValidValue

instance Show IniHeading  where show = getHeading
instance Show IniKey      where show = getKey
instance Show IniValue    where show = getValue

instance IsString IniHeading where
  fromString = fromMaybe <$> error . ("Not a heading: " ++) <*> mkHdg
instance IsString IniKey where
  fromString = fromMaybe <$> error . ("Not a key: " ++) <*> mkKey
instance IsString IniValue where
  fromString = fromMaybe <$> error . ("Not a value: " ++) <*> mkVal
