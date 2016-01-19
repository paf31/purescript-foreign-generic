module Test.Main where

import Prelude

import Data.Tuple
import Data.Either
import Data.Generic
import Data.Foreign
import Data.Foreign.Generic
import Data.Bifunctor (bimap)
import Test.Assert (assert, assert', ASSERT())

import Control.Monad.Eff
import Control.Monad.Eff.Console

-- | Balanced binary leaf trees
data Tree a = Leaf a | Branch (Tree (Tuple a a))

derive instance genericTree :: (Generic a) => Generic (Tree a)

buildTree :: forall a. (a -> Tuple a a) -> Int -> a -> Tree a
buildTree _ 0 a = Leaf a
buildTree f n a = Branch $ buildTree (bimap f f) (n - 1) (f a)

-- A balanced binary tree of depth 5
tree :: Tree Int
tree = buildTree (\i -> Tuple (2 * i) (2 * i + 1)) 5 0

opts :: Options
opts = defaultOptions { unwrapNewtypes = true, tupleAsArray = true }

readTree :: forall a. (Generic a) => String -> F (Tree a)
readTree = readJSONGeneric opts

writeTree :: forall a. (Generic a) => Tree a -> String
writeTree = toJSONGeneric opts

data WrappedArray a = WrappedArray (Array a)
derive instance genericWrappedArray :: (Generic a) => Generic (WrappedArray a)

newtype WrappedArrayN a = WrappedArrayN (Array a)
derive instance genericWrappedArrayN :: (Generic a) => Generic (WrappedArrayN a)

data TupleArray a b = TupleArray (Array (Tuple a b))
derive instance genericTupleArray :: (Generic a, Generic b) => Generic (TupleArray a b)

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  testTree
  test "hello, world"
  test 'c'
  test 1
  test 1.0
  test false

  test (Right "hi" :: Either String String)
  test (Left "hi" :: Either String String)
  test (Tuple "foo" 1)

  let arr = [Tuple "foo" 1, Tuple "bar" 2]
  test arr
  test (WrappedArray arr)
  test (WrappedArrayN arr)
  test (TupleArray arr)

testTree = do
  let json = writeTree tree
  log json
  case readTree json of
    Right tree1 -> do
      log (gShow tree1)
      assert (gEq tree tree1)
    Left err ->
      throw (show err)

test :: forall a. (Generic a) => a -> _
test thing = do
  log ""
  log ("testing: " <> gShow thing)
  log "==="
  log ""
  let json = toJSONGeneric defaultOptions thing
  log json
  case readJSONGeneric defaultOptions json :: F a of
    Right thing1 -> do
      log ("result: " <> gShow thing1)
      assert (gEq thing thing1)
    Left err ->
      throw (show err)

throw = flip assert' false
