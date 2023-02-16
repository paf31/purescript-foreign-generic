module Test.Types where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Foreign (ForeignError(..), fail, readArray, unsafeToForeign)
import Foreign.Generic (class Encode, class Decode, Options, SumEncoding(..), encode, decode, defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Class (class DecodeWithOptions, class EncodeWithOptions)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum, genericEncodeEnum)

newtype TupleArray a b = TupleArray (Tuple a b)

derive newtype instance Bifunctor TupleArray

derive instance Generic (TupleArray a b) _

instance (Show a, Show b) => Show (TupleArray a b) where
  show x = genericShow x

instance (Eq a, Eq b) => Eq (TupleArray a b) where
  eq x y = genericEq x y

instance (Decode a, Decode b) => Decode (TupleArray a b) where
  decode x = do
    arr <- readArray x
    case arr of
      [y, z] -> TupleArray <$> (Tuple <$> decode y <*> decode z)
      _ -> fail (ForeignError "Expected two array elements")

instance (Encode a, Encode b) => Encode (TupleArray a b) where
  encode (TupleArray (Tuple a b)) = unsafeToForeign [encode a, encode b]

-- | An example record
newtype RecordTest = RecordTest
  { foo :: Int
  , bar :: String
  , baz :: Char
  }

derive instance Generic RecordTest _

instance Show RecordTest where
  show x = genericShow x

instance Eq RecordTest where
  eq x y = genericEq x y

instance Decode RecordTest where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

instance Encode RecordTest where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

-- | An example of an ADT with nullary constructors
data IntList = Nil | Cons Int IntList

derive instance Generic IntList _

instance Show IntList where
  show x = genericShow x

instance Eq IntList where
  eq x y = genericEq x y

intListOptions :: Options
intListOptions =
  defaultOptions { unwrapSingleConstructors = true
                 , sumEncoding = TaggedObject { tagFieldName: "tag"
                                               , contentsFieldName: "contents"
                                               , constructorTagTransform: \tag -> case tag of
                                                                                    "Cons" -> "cOnS"
                                                                                    _ -> ""
                                               }
                 }

instance Decode IntList where
  decode x = genericDecode intListOptions x

instance Encode IntList where
  encode x = genericEncode intListOptions x

-- | Balanced binary leaf trees
data Tree a = Leaf a | Branch (Tree (TupleArray a a))

derive instance Generic (Tree a) _

instance Show a => Show (Tree a) where
  show x = genericShow x

instance Eq a => Eq (Tree a) where
  eq x y = genericEq x y

instance (DecodeWithOptions a, Decode a) => Decode (Tree a) where
  decode x = genericDecode defaultOptions x

instance (Encode a, EncodeWithOptions a) => Encode (Tree a) where
  encode x = genericEncode defaultOptions x

newtype UndefinedTest = UndefinedTest
  { a :: Maybe String
  }

derive instance Eq UndefinedTest
derive instance Generic UndefinedTest _

instance Decode UndefinedTest where
  decode = genericDecode $ defaultOptions
instance Encode UndefinedTest where
  encode = genericEncode $ defaultOptions

data Fruit
  = Apple
  | Banana
  | Frikandel

derive instance Eq Fruit
derive instance Generic Fruit _

instance Decode Fruit where
  decode = genericDecodeEnum defaultGenericEnumOptions
instance Encode Fruit where
  encode = genericEncodeEnum defaultGenericEnumOptions
