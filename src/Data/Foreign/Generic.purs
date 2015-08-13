module Data.Foreign.Generic where
    
import Prelude

import Data.Maybe.Unsafe (fromJust)
import Data.Array (zipWithA)
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Index
import Data.Generic
import Data.Foldable (foldl)
import Data.Traversable (for)

import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Bind ((>=>))
import Control.Monad (when)
    
-- | Read a value which has a `Generic` type.
readGeneric :: forall a. (Generic a) => Foreign -> F a
readGeneric = map (fromJust <<< fromSpine) <<< go (toSignature (anyProxy :: Proxy a))
  where
  go :: GenericSignature -> Foreign -> F GenericSpine
  go SigNumber f = map SNumber (readNumber f)
  go SigInt f = map SInt (readInt f)
  go SigString f = map SString (readString f)
  go (SigArray el) f = do arr <- readArray f
                          els <- for arr \f -> do
                            e <- go (el unit) f
                            return (const e)
                          return (SArray els)
  go (SigRecord props) f = do fs <- for props \prop -> do pf <- f ! prop.recLabel
                                                          sp <- go (prop.recValue unit) pf
                                                          return { recLabel: prop.recLabel, recValue: const sp }
                              return (SRecord fs)      
  go (SigProd alts) f = foldl (<|>) (Left (TypeMismatch "product type" "invalid constructor")) (map (`readCtor` f) alts)    
  
  readCtor :: { sigConstructor :: String, sigValues :: Array (Unit -> GenericSignature) } -> Foreign -> F GenericSpine     
  readCtor ctor f = do
    tag <- prop "tag" f >>= readString
    when (tag /= ctor.sigConstructor) (Left (TypeMismatch ctor.sigConstructor tag))
    vals <- prop "values" f >>= readArray
    sps <- zipWithA (\k -> go (k unit)) ctor.sigValues vals
    return (SProd tag (map const sps))
    
    
-- | Read a value which has a `Generic` type from a JSON String
readJSONGeneric :: forall a. (Generic a) => String -> F a
readJSONGeneric = parseJSON >=> readGeneric