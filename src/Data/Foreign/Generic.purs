module Data.Foreign.Generic where

import Prelude

import Data.Maybe
import Data.Array (zipWithA)
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Index
import Data.Generic
import Data.Foldable (find)
import Data.Traversable (for)

import Control.Bind ((>=>))

type Options =
  { sumEncoding :: SumEncoding
  , unwrapNewtypes :: Boolean
  , unwrapSingleArgumentConstructors :: Boolean
  , maybeAsNull :: Boolean
  }

data SumEncoding
  = TaggedObject
    { tagFieldName      :: String
    , contentsFieldName :: String
    }

defaultOptions :: Options
defaultOptions =
  { sumEncoding: TaggedObject
                   { tagFieldName: "tag"
                   , contentsFieldName: "contents"
                   }
  , unwrapNewtypes: false
  , unwrapSingleArgumentConstructors: true
  , maybeAsNull: true
  }

-- | Read a value which has a `Generic` type.
readGeneric :: forall a. (Generic a) => Options -> Foreign -> F a
readGeneric { sumEncoding, unwrapNewtypes, unwrapSingleArgumentConstructors, maybeAsNull } =
  go (toSignature (anyProxy :: Proxy a)) >=>
    fromSpine >>> maybe (Left (TypeMismatch "valid spine" "invalid spine")) Right
  where
  go :: GenericSignature -> Foreign -> F GenericSpine
  go SigNumber f = map SNumber (readNumber f)
  go SigInt f = map SInt (readInt f)
  go SigChar f = map SChar (readChar f)
  go SigString f = map SString (readString f)
  go SigBoolean f = map SBoolean (readBoolean f)
  go (SigArray el) f = do
    arr <- readArray f
    els <- for arr \f -> do
      e <- go (el unit) f
      return (const e)
    return (SArray els)
  go (SigRecord props) f = do
    fs <- for props \prop -> do
      pf <- f ! prop.recLabel
      sp <- go (prop.recValue unit) pf
      return { recLabel: prop.recLabel, recValue: const sp }
    return (SRecord fs)
  go (SigProd _ [{ sigConstructor: tag, sigValues: [sig] }]) f | unwrapNewtypes = do
    sp <- go (sig unit) f
    return (SProd tag [\_ -> sp])
  go (SigProd "Data.Maybe.Maybe" [{ sigValues: [just] }, _]) f | maybeAsNull = do
    if isNull f
      then return (SProd "Data.Maybe.Nothing" [])
      else do sp <- go (just unit) f
              return (SProd "Data.Maybe.Just" [\_ -> sp])
  go (SigProd _ alts) f =
    case sumEncoding of
      TaggedObject { tagFieldName, contentsFieldName } -> do
        tag <- prop tagFieldName f >>= readString
        case find (\alt -> alt.sigConstructor == tag) alts of
          Nothing -> Left (TypeMismatch ("one of " <> show (map _.sigConstructor alts)) tag)
          Just { sigValues: [] } -> return (SProd tag [])
          Just { sigValues: [sig] } | unwrapSingleArgumentConstructors -> do
            val <- prop contentsFieldName f
            sp <- go (sig unit) val
            return (SProd tag [\_ -> sp])
          Just { sigValues } -> do
            vals <- prop contentsFieldName f >>= readArray
            sps <- zipWithA (\k -> go (k unit)) sigValues vals
            return (SProd tag (map const sps))

-- | Read a value which has a `Generic` type from a JSON String
readJSONGeneric :: forall a. (Generic a) => Options -> String -> F a
readJSONGeneric opts = parseJSON >=> readGeneric opts
