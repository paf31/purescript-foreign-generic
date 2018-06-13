module Foreign.JSON
  ( parseJSON
  , decodeJSONWith
  ) where

import Control.Monad.Except (ExceptT(..))
import Data.Bifunctor (lmap)
import Data.Identity (Identity(..))
import Effect.Exception (message, try)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign, ForeignError(..), F)
import Prelude

foreign import parseJSONImpl :: EffectFn1 String Foreign

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
