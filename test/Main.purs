module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), fromRight)
import Data.Foreign (F, Foreign, ForeignError(..))
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Generic (Options, SumEncoding(..), defaultOptions, readJSONGeneric, toJSONGeneric, readGeneric)
import Data.Generic (class Generic, toSignature, toSpine, fromSpine, gEq, gShow, GenericSpine(..), GenericSignature(..))
import Data.Maybe (Maybe(..))
import Data.String (lastIndexOf, drop, toLower)
import Data.String.Regex (regex, noFlags, replace)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assert', ASSERT)

-- | Balanced binary leaf trees
data Tree a = Leaf a | Branch (Tree (Tuple a a))

derive instance genericTree :: (Generic a) => Generic (Tree a)

buildTree :: forall a. (a -> Tuple a a) -> Int -> a -> Tree a
buildTree _ 0 a = Leaf a
buildTree f n a = Branch $ buildTree (bimap f f) (n - 1) (f a)

-- A balanced binary tree of depth 5
tree :: Tree Int
tree = buildTree (\i -> Tuple (2 * i) (2 * i + 1)) 5 0

readTree :: forall a. (Generic a) => Options -> String -> F (Tree a)
readTree opts = readJSONGeneric opts

writeTree :: forall a. (Generic a) => Options -> Tree a -> String
writeTree opts = toJSONGeneric opts

data WrappedArray a = WrappedArray (Array a)
derive instance genericWrappedArray :: (Generic a) => Generic (WrappedArray a)

newtype WrappedArrayN a = WrappedArrayN (Array a)
derive instance genericWrappedArrayN :: (Generic a) => Generic (WrappedArrayN a)

data TupleArray a b = TupleArray (Array (Tuple a b))
derive instance genericTupleArray :: (Generic a, Generic b) => Generic (TupleArray a b)

newtype WrappedRecord
  = WrappedRecord
    { propFoo :: String
    , propBAR :: Int
    , order :: Ordering
    }
derive instance genericWrappedRecord :: Generic WrappedRecord

shortNames :: String -> String
shortNames s =
  case lastIndexOf "." s of
    Nothing -> s
    Just i -> drop (i + 1) s

camelTo :: String -> String -> String
camelTo to str =
  toLower (replace rx "_$1" str)
  where
    rx = unsafePartial (fromRight (regex "([A-Z]+)" opts))
    opts = noFlags { global = true }

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  test defaultOptions
  test defaultOptions { untagEnums = true, constructorTagModifier = shortNames }
  test defaultOptions { untagEnums = true, fieldLabelModifier = camelTo "_" }

test :: forall eff. Options -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
test opts = do
  testTree opts
  test "hello, world"
  test 'c'
  test 1
  test 1.0
  test false
  test GT

  test (Right "hi" :: Either String String)
  test (Left "hi" :: Either String String)
  test (Tuple "fooBar" 1)

  let arr = [Tuple "fooBar" 1, Tuple "baz" 2]
  test arr
  test (WrappedArray arr)
  test (WrappedArrayN arr)
  test (TupleArray arr)
  test (WrappedRecord { propFoo: "hi", propBAR: 3, order: GT })

  where
    test :: forall a. Generic a
         => a
         -> Eff ( console :: CONSOLE , assert :: ASSERT | eff) Unit
    test = test' opts

testTree
  :: forall eff
   . Options
  -> Eff ( console :: CONSOLE
         , assert :: ASSERT
         | eff
         ) Unit
testTree opts = do
  let json = writeTree opts tree
  log json
  case readTree opts json of
    Right tree1 -> do
      log (gShow tree1)
      assert (gEq tree tree1)
    Left err ->
      throw (show err)

test'
  :: forall a eff
   . Generic a
  => Options
  -> a
  -> Eff ( console :: CONSOLE
         , assert :: ASSERT
         | eff
         ) Unit
test' opts thing = do
  log ""
  log ("testing: " <> gShow thing)
  log "==="
  log ""
  let json = toJSONGeneric opts thing
  log json
  case readJSONGeneric opts json :: F a of
    Right thing1 -> do
      log ("result: " <> gShow thing1)
      assert (gEq thing thing1)
    Left err ->
      throw (show err)

throw :: forall eff. String -> Eff (assert :: ASSERT | eff) Unit
throw = flip assert' false
