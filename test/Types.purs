module Test.Types where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Foreign (ForeignError(ForeignError), fail, readArray, toForeign)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (Tuple(..))

newtype TupleArray a b = TupleArray (Tuple a b)

derive newtype instance bifunctorTupleArray :: Bifunctor TupleArray

derive instance genericTupleArray :: Generic (TupleArray a b) _

instance showTupleArray :: (Show a, Show b) => Show (TupleArray a b) where
  show x = genericShow x

instance eqTupleArray :: (Eq a, Eq b) => Eq (TupleArray a b) where
  eq x y = genericEq x y

instance decodeTupleArray :: (Decode a, Decode b) => Decode (TupleArray a b) where
  decode x = do
    arr <- readArray x
    case arr of
      [y, z] -> TupleArray <$> (Tuple <$> decode y <*> decode z)
      _ -> fail (ForeignError "Expected two array elements")

instance encodeTupleArray :: (Encode a, Encode b) => Encode (TupleArray a b) where
  encode (TupleArray (Tuple a b)) = toForeign [encode a, encode b]

-- | An example record
newtype RecordTest = RecordTest
  { foo :: Int
  , bar :: String
  , baz :: Char
  }

derive instance genericRecordTest :: Generic RecordTest _

instance showRecordTest :: Show RecordTest where
  show x = genericShow x

instance eqRecordTest :: Eq RecordTest where
  eq x y = genericEq x y

instance decodeRecordTest :: Decode RecordTest where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

instance encodeRecordTest :: Encode RecordTest where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

-- | An example of an ADT with nullary constructors
data IntList = Nil | Cons Int IntList

derive instance genericIntList :: Generic IntList _

instance showIntList :: Show IntList where
  show x = genericShow x

instance eqIntList :: Eq IntList where
  eq x y = genericEq x y

instance decodeIntList :: Decode IntList where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

instance encodeIntList :: Encode IntList where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

-- | Balanced binary leaf trees
data Tree a = Leaf a | Branch (Tree (TupleArray a a))

derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show x = genericShow x

instance eqTree :: Eq a => Eq (Tree a) where
  eq x y = genericEq x y

instance decodeTree :: Decode a => Decode (Tree a) where
  decode x = genericDecode defaultOptions x

instance encodeTree :: Encode a => Encode (Tree a) where
  encode x = genericEncode defaultOptions x

newtype UndefinedTest = UndefinedTest
  { a :: NullOrUndefined String
  }

derive instance eqUT :: Eq UndefinedTest
derive instance geUT :: Generic UndefinedTest _

instance dUT :: Decode UndefinedTest where
  decode = genericDecode $ defaultOptions
instance eUT :: Encode UndefinedTest where
  encode = genericEncode $ defaultOptions

data Fruit
  = Apple
  | Banana
  | Frikandel

derive instance eqFruit :: Eq Fruit
derive instance geFruit :: Generic Fruit _

instance dFruit :: Decode Fruit where
  decode = genericDecodeEnum defaultGenericEnumOptions
instance eFruit :: Encode Fruit where
  encode = genericEncodeEnum defaultGenericEnumOptions
