module Data.Foreign.Generic where

import Prelude

import Control.Bind ((>=>))
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array (zipWith, zipWithA, sortBy)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Foreign (F, Foreign, ForeignError(..), parseJSON, toForeign, readArray,
                     readString, isUndefined, isNull, readBoolean, readChar, readInt,
                     readNumber)
import Data.Foreign.Index (prop, (!))
import Data.Function (on)
import Data.Generic (class Generic, GenericSignature(..), GenericSpine(..), toSpine,
                     toSignature, fromSpine)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.StrMap as S
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Type.Proxy (Proxy(..))

type Options =
  { sumEncoding :: SumEncoding
  , unwrapNewtypes :: Boolean
  , unwrapSingleArgumentConstructors :: Boolean
  , maybeAsNull :: Boolean
  , tupleAsArray :: Boolean
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
  , tupleAsArray: false
  }

-- | Read a value which has a `Generic` type.
readGeneric :: forall a. Generic a => Options -> Foreign -> F a
readGeneric { sumEncoding
            , unwrapNewtypes
            , unwrapSingleArgumentConstructors
            , maybeAsNull
            , tupleAsArray
            } = map fromSpineUnsafe <<< go (toSignature (Proxy :: Proxy a))
  where
  fromSpineUnsafe :: GenericSpine -> a
  fromSpineUnsafe sp =
    case fromSpine sp of
      Nothing -> unsafeThrow "Invalid spine for signature"
      Just a -> a

  go :: GenericSignature -> Foreign -> F GenericSpine
  go SigUnit _ = pure SUnit
  go SigNumber f = map SNumber (readNumber f)
  go SigInt f = map SInt (readInt f)
  go SigChar f = map SChar (readChar f)
  go SigString f = map SString (readString f)
  go SigBoolean f = map SBoolean (readBoolean f)
  go (SigArray el) f = do
    arr <- readArray f
    els <- for arr \f -> do
      e <- go (el unit) f
      pure (const e)
    pure (SArray els)
  go (SigRecord props) f = do
    fs <- for props \prop -> do
      pf <- f ! prop.recLabel
      sp <- go (prop.recValue unit) pf
      pure { recLabel: prop.recLabel, recValue: const sp }
    pure (SRecord fs)
  go (SigProd _ [{ sigConstructor: tag, sigValues: [sig] }]) f | unwrapNewtypes = do
    sp <- go (sig unit) f
    pure (SProd tag [\_ -> sp])
  go (SigProd "Data.Maybe.Maybe" [{ sigValues: [just] }, _]) f | maybeAsNull = do
    if isNull f || isUndefined f
      then pure (SProd "Data.Maybe.Nothing" [])
      else do sp <- go (just unit) f
              pure (SProd "Data.Maybe.Just" [\_ -> sp])
  go (SigProd "Data.Tuple.Tuple" [{ sigValues: [_1, _2] }]) f | tupleAsArray = do
    arr <- readArray f
    case arr of
      [a, b] -> do
        x <- go (_1 unit) a
        y <- go (_2 unit) b
        pure $ SProd "Data.Tuple.Tuple" [\_ -> x, \_ -> y]
      _ -> Left (TypeMismatch "array of length 2" "array")
  go (SigProd _ alts) f =
    case sumEncoding of
      TaggedObject { tagFieldName, contentsFieldName } -> do
        tag <- prop tagFieldName f >>= readString
        case find (\alt -> alt.sigConstructor == tag) alts of
          Nothing -> Left (TypeMismatch ("one of " <> show (map _.sigConstructor alts)) tag)
          Just { sigValues: [] } -> pure (SProd tag [])
          Just { sigValues: [sig] } | unwrapSingleArgumentConstructors -> do
            val <- prop contentsFieldName f
            sp <- go (sig unit) val
            pure (SProd tag [\_ -> sp])
          Just { sigValues } -> do
            vals <- prop contentsFieldName f >>= readArray
            sps <- zipWithA (\k -> go (k unit)) sigValues vals
            pure (SProd tag (map const sps))

-- | Generate a `Foreign` value compatible with the `readGeneric` function.
toForeignGeneric :: forall a. (Generic a) => Options -> a -> Foreign
toForeignGeneric { sumEncoding
                 , unwrapNewtypes
                 , unwrapSingleArgumentConstructors
                 , maybeAsNull
                 , tupleAsArray
                 } = go (toSignature (Proxy :: Proxy a)) <<< toSpine
  where
  go :: GenericSignature -> GenericSpine -> Foreign
  go _ (SNumber n)  = toForeign n
  go _ (SInt i)     = toForeign i
  go _ (SChar c)    = toForeign c
  go _ (SString s)  = toForeign s
  go _ (SBoolean b) = toForeign b
  go (SigArray sig) (SArray arr) = toForeign (map (go (sig unit) <<< (_ $ unit)) arr)
  go (SigRecord sigs) (SRecord sps) = toForeign (S.fromList (L.fromFoldable pairs))
    where
    pairs :: Array (Tuple String Foreign)
    pairs = zipWith pair (sortBy (compare `on` _.recLabel) sigs)
                         (sortBy (compare `on` _.recLabel) sps)

    pair sig sp | sig.recLabel == sp.recLabel = Tuple sig.recLabel (go (sig.recValue unit) (sp.recValue unit))
                | otherwise = unsafeThrow "Record fields do not match signature"
  go (SigProd "Data.Maybe.Maybe" _) (SProd "Data.Maybe.Nothing" []) | maybeAsNull = toForeign (toNullable Nothing)
  go (SigProd "Data.Maybe.Maybe" [{ sigValues: [just] }, _]) (SProd "Data.Maybe.Just" [sp]) | maybeAsNull = go (just unit) (sp unit)
  go (SigProd "Data.Tuple.Tuple" [{ sigValues: [_1, _2] }]) (SProd "Data.Tuple.Tuple" [a, b]) | tupleAsArray = do
    toForeign [ go (_1 unit) (a unit), go (_2 unit) (b unit) ]
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
                    toForeign (S.fromList (L.fromFoldable [ Tuple tagFieldName (toForeign tag)
                                                          , Tuple contentsFieldName f
                                                          ]))
              fs -> toForeign (S.fromList (L.fromFoldable [ Tuple tagFieldName (toForeign tag)
                                                          , Tuple contentsFieldName (toForeign fs)
                                                          ]))
  go _ _ = unsafeThrow "Invalid spine for signature"

-- | Read a value which has a `Generic` type from a JSON String
readJSONGeneric :: forall a. (Generic a) => Options -> String -> F a
readJSONGeneric opts = parseJSON >=> readGeneric opts

-- | Write a value which has a `Generic` type as a JSON String
toJSONGeneric :: forall a. (Generic a) => Options -> a -> String
toJSONGeneric opts = toForeignGeneric opts >>> unsafeStringify
