module Foreign.JSON
  ( parseJSON
  , decodeJSONWith
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.Bifunctor (lmap)
import Foreign (Foreign, ForeignError(..), F)
import Data.Identity (Identity(..))
import Effect.Exception (message, try)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)

foreign import parseJSONImpl :: EffectFn1 String Foreign

-- | Parse a JSON string as `Foreign` data
parseJSON :: String -> F Foreign
parseJSON =
  ExceptT
  <<< Identity
  <<< lmap (pure <<< ForeignError <<< message)
  <<< runPure
  <<< try
  <<< runEffectFn1 parseJSONImpl
  where
    -- we have sufficiently caught the error from the effect here
    runPure = unsafePerformEffect

decodeJSONWith :: forall a. (Foreign -> F a) -> String -> F a
decodeJSONWith f = f <=< parseJSON
