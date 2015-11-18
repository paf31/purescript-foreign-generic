module Data.Foreign.Generic where

import Prelude

import Data.Maybe
import Data.Array (zipWith, zipWithA, sortBy)
import Data.Tuple
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Index
import Data.Function (on)
import Data.Nullable (toNullable)
import Data.Generic
import Data.Foldable (find)
import Data.Traversable (for)
import Data.List as L
import Data.StrMap as S

import Control.Bind ((>=>))
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import Global.Unsafe (unsafeStringify)

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
  map fromSpineUnsafe <<< go (toSignature (anyProxy :: Proxy a))
  where
  fromSpineUnsafe :: GenericSpine -> a
  fromSpineUnsafe sp =
    case fromSpine sp of
      Nothing -> unsafeThrow "Invalid spine for signature"
      Just a -> a

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
    if isNull f || isUndefined f
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

-- | Generate a `Foreign` value compatible with the `readGeneric` function.
toForeignGeneric :: forall a. (Generic a) => Options -> a -> Foreign
toForeignGeneric { sumEncoding, unwrapNewtypes, unwrapSingleArgumentConstructors, maybeAsNull } = go (toSignature (Proxy :: Proxy a)) <<< toSpine
  where
  go :: GenericSignature -> GenericSpine -> Foreign
  go _ (SNumber n)  = toForeign n
  go _ (SInt i)     = toForeign i
  go _ (SChar c)    = toForeign c
  go _ (SString s)  = toForeign s
  go _ (SBoolean b) = toForeign b
  go (SigArray sig) (SArray arr) = toForeign (map (go (sig unit) <<< ($ unit)) arr)
  go (SigRecord sigs) (SRecord sps) = toForeign (S.fromList (L.toList pairs))
    where
    pairs :: Array (Tuple String Foreign)
    pairs = zipWith pair (sortBy (compare `on` _.recLabel) sigs)
                         (sortBy (compare `on` _.recLabel) sps)

    pair sig sp | sig.recLabel == sp.recLabel = Tuple sig.recLabel (go (sig.recValue unit) (sp.recValue unit))
                | otherwise = unsafeThrow "Record fields do not match signature"
  go (SigProd "Data.Maybe.Maybe" _) (SProd "Data.Maybe.Nothing" []) | maybeAsNull = toForeign (toNullable Nothing)
  go (SigProd "Data.Maybe.Maybe" [{ sigValues: [just] }, _]) (SProd "Data.Maybe.Just" [sp]) | maybeAsNull = go (just unit) (sp unit)
  go (SigProd _ [{ sigConstructor: _, sigValues: [sig] }]) (SProd _ [sp]) | unwrapNewtypes = go (sig unit) (sp unit)
  go (SigProd _ alts) (SProd tag sps) =
    case sumEncoding of
      TaggedObject { tagFieldName, contentsFieldName } ->
        case find (\alt -> alt.sigConstructor == tag) alts of
          Nothing -> unsafeThrow ("No signature for data constructor " <> tag)
          Just { sigValues } ->
            case zipWith (\sig sp -> go (sig unit) (sp unit)) sigValues sps of
              [] -> toForeign (S.fromList (L.singleton (Tuple tagFieldName (toForeign tag))))
              [f] | unwrapSingleArgumentConstructors ->
                    toForeign (S.fromList (L.toList [ Tuple tagFieldName (toForeign tag)
                                                    , Tuple contentsFieldName f
                                                    ]))
              fs -> toForeign (S.fromList (L.toList [ Tuple tagFieldName (toForeign tag)
                                                    , Tuple contentsFieldName (toForeign fs)
                                                    ]))
  go _ _ = unsafeThrow "Invalid spine for signature"

-- | Read a value which has a `Generic` type from a JSON String
readJSONGeneric :: forall a. (Generic a) => Options -> String -> F a
readJSONGeneric opts = parseJSON >=> readGeneric opts

-- | Write a value which has a `Generic` type as a JSON String
toJSONGeneric :: forall a. (Generic a) => Options -> a -> String
toJSONGeneric opts = toForeignGeneric opts >>> unsafeStringify
