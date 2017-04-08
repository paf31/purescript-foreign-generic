module Data.Foreign.JSON
  ( parseJSON
  , decodeJSONWith
  ) where

import Prelude
import Control.Monad.Eff (runPure)
import Control.Monad.Eff.Exception (EXCEPTION, message, try)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Control.Monad.Except (ExceptT(..))
import Data.Bifunctor (lmap)
import Data.Foreign (Foreign, ForeignError(..), F)
import Data.Identity (Identity(..))

foreign import parseJSONImpl :: forall eff. EffFn1 (exception :: EXCEPTION | eff) String Foreign

-- | Parse a JSON string as `Foreign` data
parseJSON :: String -> F Foreign
parseJSON =
  ExceptT
  <<< Identity
  <<< lmap (pure <<< JSONError <<< message)
  <<< runPure
  <<< try
  <<< runEffFn1 parseJSONImpl

decodeJSONWith :: forall a. (Foreign -> F a) -> String -> F a
decodeJSONWith f = f <=< parseJSON
