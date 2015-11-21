module Test.Main where

import Prelude

import Data.Tuple
import Data.Either
import Data.Generic
import Data.Foreign
import Data.Foreign.Generic
import Data.Bifunctor (bimap)

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

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  let json = writeTree tree
  log json
  case readTree json of
    Right tree1 -> do
      log (gShow tree1)
      print (gEq tree tree1)
    Left err -> print err
