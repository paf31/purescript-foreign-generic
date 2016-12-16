module Test.Types where

import Prelude
import Data.Bifunctor (class Bifunctor)
import Data.Foreign (ForeignError(ForeignError), fail, readArray, toForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, write)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
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

instance isForeignTupleArray :: (IsForeign a, IsForeign b) => IsForeign (TupleArray a b) where
  read x = do
    arr <- readArray x
    case arr of
      [y, z] -> TupleArray <$> (Tuple <$> read y <*> read z)
      _ -> fail (ForeignError "Expected two array elements")

instance asForeignTupleArray :: (AsForeign a, AsForeign b) => AsForeign (TupleArray a b) where
  write (TupleArray (Tuple a b)) = toForeign [write a, write b]

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

instance isForeignRecordTest :: IsForeign RecordTest where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

instance asForeignRecordTest :: AsForeign RecordTest where
  write x = toForeignGeneric (defaultOptions { unwrapSingleConstructors = true }) x

-- | An example of an ADT with nullary constructors
data IntList = Nil | Cons Int IntList

derive instance genericIntList :: Generic IntList _

instance showIntList :: Show IntList where
  show x = genericShow x

instance eqIntList :: Eq IntList where
  eq x y = genericEq x y

instance isForeignIntList :: IsForeign IntList where
  read x = readGeneric (defaultOptions { unwrapSingleConstructors = true }) x

instance asForeignIntList :: AsForeign IntList where
  write x = toForeignGeneric (defaultOptions { unwrapSingleConstructors = true }) x

-- | Balanced binary leaf trees
data Tree a = Leaf a | Branch (Tree (TupleArray a a))

derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show x = genericShow x

instance eqTree :: Eq a => Eq (Tree a) where
  eq x y = genericEq x y

instance isForeignTree :: IsForeign a => IsForeign (Tree a) where
  read x = readGeneric defaultOptions x

instance asForeignTree :: AsForeign a => AsForeign (Tree a) where
  write x = toForeignGeneric defaultOptions x
