module Test.Main where

import Prelude

import Data.Maybe
import Data.Either
import Data.Generic
import Data.Foreign
import Data.Foreign.Generic

import Control.Monad.Eff
import Control.Monad.Eff.Console

data Tree a = Leaf a | Branch (Tree a) (Tree a)

derive instance genericTree :: (Generic a) => Generic (Tree a)

json :: String
json = """
  {
    "tag": "Test.Main.Branch",
    "contents": [
        {
            "tag": "Test.Main.Leaf",
            "contents": true
        },
        {
            "tag": "Test.Main.Leaf",
            "contents": null
        }
    ]
}"""

readTree :: forall a. (Generic a) => String -> F (Tree a)
readTree = readJSONGeneric defaultOptions

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  case readTree json :: F (Tree (Maybe Boolean)) of
    Right tree -> log (gShow tree)
    Left err -> print err
