module Test.Main where

import Prelude

import Data.Either
import Data.Generic
import Data.Foreign
import Data.Foreign.Generic

import Control.Monad.Eff.Console

data Tree a = Leaf a | Branch (Tree a) (Tree a)

derive instance genericTree :: (Generic a) => Generic (Tree a)

json :: String
json = """
  {
    "tag": "Branch",
    "values": [
        {
            "tag": "Leaf",
            "values": [
                1
            ]
        },
        {
            "tag": "Leaf",
            "values": [
                2
            ]
        }
    ]
}

  """

readTree :: forall a. (Generic a) => String -> F (Tree a)
readTree = readJSONGeneric

main = do
  case readTree json :: F (Tree Int) of
    Right tree -> log (gShow tree)
    Left err -> print err
