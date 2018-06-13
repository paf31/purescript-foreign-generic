module Data.Foreign.JSON
  ( parseJSON
  , decodeJSONWith
  , addToObj
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.Bifunctor (lmap)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Identity (Identity(..))
import Effect.Exception (message, try)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign, ForeignError(..), F)

foreign import parseJSONImpl :: EffectFn1 String Foreign

foreign import addToObjImpl :: Fn3 String Foreign Foreign Foreign

addToObj :: String -> Foreign -> Foreign -> Foreign
addToObj = runFn3 addToObjImpl

-- | Parse a JSON string as `Foreign` data
parseJSON :: String -> F Foreign
parseJSON =
  ExceptT
  <<< Identity
  <<< lmap (pure <<< ForeignError <<< message)
  <<< unsafePerformEffect
  <<< try 
  <<< runEffectFn1 parseJSONImpl

decodeJSONWith :: forall a. (Foreign -> F a) -> String -> F a
decodeJSONWith f = f <=< parseJSON
