module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foreign.Class (class AsForeign, class IsForeign, readJSON, write)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Test.Assert (assert, assert', ASSERT)
import Test.Types (IntList(..), RecordTest(..), Tree(..), TupleArray(..))

buildTree :: forall a. (a -> TupleArray a a) -> Int -> a -> Tree a
buildTree _ 0 a = Leaf a
buildTree f n a = Branch $ buildTree (bimap f f) (n - 1) (f a)

-- A balanced binary tree of depth N
makeTree :: Int -> Tree Int
makeTree n = buildTree (\i -> TupleArray (Tuple (2 * i) (2 * i + 1))) n 0

throw :: forall eff. String -> Eff (assert :: ASSERT | eff) Unit
throw = flip assert' false

testRoundTrip
  :: ∀ a eff
   . ( Eq a
     , IsForeign a
     , AsForeign a
     )
  => a
  -> Eff ( console :: CONSOLE
         , assert :: ASSERT
         | eff
         ) Unit
testRoundTrip x = do
  let json = unsafeStringify (write x)
  log json
  case runExcept (readJSON json) of
    Right y -> assert (x == y)
    Left err -> throw (show err)

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  testRoundTrip (RecordTest { foo: 1, bar: "test", baz: 'a' })
  testRoundTrip (Cons 1 (Cons 2 (Cons 3 Nil)))
  testRoundTrip (makeTree 0)
  testRoundTrip (makeTree 5)
