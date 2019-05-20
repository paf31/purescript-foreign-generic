module Foreign.Class where

import Prelude

import Control.Monad.Except (except, mapExcept)
import Data.Array ((..), zipWith, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Foreign (F, Foreign, ForeignError(..), readArray, readBoolean, readChar, readInt, readNumber, readString, unsafeToForeign)
import Foreign.Internal (readObject)
import Foreign.NullOrUndefined (readNullOrUndefined, null)
import Foreign.Object (Object)
import Foreign.Object as Object
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Index (index)
import Record as Record
import Record.Builder as Builder
import Record.Builder (Builder)
import Unsafe.Coerce (unsafeCoerce)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons)
import Type.Data.RowList (RLProxy(..))
import Foreign.Generic.Types (Options, SumEncoding(..))

-- | Default decoding/encoding options:
-- |
-- | - Represent sum types as records with `tag` and `contents` fields
-- | - Unwrap single arguments
-- | - Don't unwrap single constructors
-- | - Use the constructor names as-is
-- | - Use the field names as-is
defaultOptions :: Options
defaultOptions =
  { sumEncoding:
      TaggedObject
        { tagFieldName: "tag"
        , contentsFieldName: "contents"
        , constructorTagTransform: identity
        }
  , unwrapSingleConstructors: false
  , unwrapSingleArguments: true
  , fieldTransform: identity
  }

-- | The `Decode` class is used to generate decoding functions
-- | of the form `Foreign -> F a` using `generics-rep` deriving.
-- |
-- | First, derive `Generic` for your data:
-- |
-- | ```purescript
-- | import Data.Generic.Rep
-- |
-- | data MyType = MyType ...
-- |
-- | derive instance genericMyType :: Generic MyType _
-- | ```
-- |
-- | You can then use the `genericDecode` and `genericDecodeJSON` functions
-- | to decode your foreign/JSON-encoded data.
class Decode a where
  decode :: Foreign -> F a

instance voidDecode :: Decode Void where
  decode _ = except (Left (pure (ForeignError "Decode: void")))

instance unitDecode :: Decode Unit where
  decode _ = pure unit

instance foreignDecode :: Decode Foreign where
  decode = pure

instance stringDecode :: Decode String where
  decode = readString

instance charDecode :: Decode Char where
  decode = readChar

instance booleanDecode :: Decode Boolean where
  decode = readBoolean

instance numberDecode :: Decode Number where
  decode = readNumber

instance intDecode :: Decode Int where
  decode = readInt

instance identityDecode :: Decode a => Decode (Identity a) where
  decode = map Identity <<< decode

instance arrayDecode :: Decode a => Decode (Array a) where
  decode = readArray >=> readElements where
    readElements :: Array Foreign -> F (Array a)
    readElements arr = sequence (zipWith readElement (0 .. length arr) arr)

    readElement :: Int -> Foreign -> F a
    readElement i value = mapExcept (lmap (map (ErrorAtIndex i))) (decode value)

instance maybeDecode :: Decode a => Decode (Maybe a) where
  decode = readNullOrUndefined decode

instance objectDecode :: Decode v => Decode (Object v) where
  decode = sequence <<< Object.mapWithKey (\_ -> decode) <=< readObject

instance recordDecode :: (RowToList r rl, DecodeRecord r rl) => Decode (Record r) where
  decode = decode_ defaultOptions


-- | The `Encode` class is used to generate encoding functions
-- | of the form `a -> Foreign` using `generics-rep` deriving.
-- |
-- | First, derive `Generic` for your data:
-- |
-- | ```purescript
-- | import Data.Generic.Rep
-- |
-- | data MyType = MyType ...
-- |
-- | derive instance genericMyType :: Generic MyType _
-- | ```
-- |
-- | You can then use the `genericEncode` and `genericEncodeJSON` functions
-- | to encode your data as JSON.
class Encode a where
  encode :: a -> Foreign

instance voidEncode :: Encode Void where
  encode = absurd

instance unitEncode :: Encode Unit where
  encode _ = unsafeToForeign {}

instance foreignEncode :: Encode Foreign where
  encode = identity

instance stringEncode :: Encode String where
  encode = unsafeToForeign

instance charEncode :: Encode Char where
  encode = unsafeToForeign

instance booleanEncode :: Encode Boolean where
  encode = unsafeToForeign

instance numberEncode :: Encode Number where
  encode = unsafeToForeign

instance intEncode :: Encode Int where
  encode = unsafeToForeign

instance identityEncode :: Encode a => Encode (Identity a) where
  encode = encode <<< unwrap

instance arrayEncode :: Encode a => Encode (Array a) where
  encode = unsafeToForeign <<< map encode

instance maybeEncode :: Encode a => Encode (Maybe a) where
  encode = maybe null encode

instance objectEncode :: Encode v => Encode (Object v) where
  encode = unsafeToForeign <<< Object.mapWithKey (\_ -> encode)

instance recordEncode :: (RowToList r rl, EncodeRecord r rl) => Encode (Record r) where
  encode = encode_ defaultOptions

class Decode_ a where
  decode_ :: Options -> Foreign -> F a

class Encode_ a where
  encode_ :: Options -> a -> Foreign

instance decode_Record :: (RowToList r rl, DecodeRecord r rl) => Decode_ (Record r) where
  decode_ opts = map (flip Builder.build {}) <$> decodeRecord_ (RLProxy :: RLProxy rl) opts
else instance decode_Other :: Decode a => Decode_ a where
  decode_ _ = decode

instance encode_Record :: (RowToList r rl, EncodeRecord r rl) => Encode_ (Record r) where
  encode_ opts = unsafeToForeign <<< encodeRecord_ (RLProxy :: RLProxy rl) opts
else instance encode_Other :: Encode a => Encode_ a where
  encode_ _ = encode

class DecodeRecord r rl | rl -> r where
  decodeRecord_ :: RLProxy rl -> Options -> Foreign -> F (Builder {} (Record r))

class EncodeRecord r rl | rl -> r where
  encodeRecord_ :: RLProxy rl -> Options -> Record r -> Object Foreign

instance decodeRecordNil :: DecodeRecord () Nil where
  decodeRecord_ _ _ _ = pure identity

instance encodeRecordNil :: EncodeRecord () Nil where
  encodeRecord_ _ _ _ = Object.empty

instance decodeRecordCons
    :: ( Cons l a r_ r
       , DecodeRecord r_ rl_
       , IsSymbol l
       , Decode_ a
       , Lacks l r_
       )
    => DecodeRecord r (Cons l a rl_)
  where
    decodeRecord_ _ opts f = do
      builder <- decodeRecord_ (RLProxy :: RLProxy rl_) opts f
      let l = reflectSymbol (SProxy :: SProxy l)
          l_transformed = (opts.fieldTransform l)
      f_ <- index f l_transformed
      a <- mapExcept (lmap (map (ErrorAtProperty l_transformed))) (decode_ opts f_)
      pure (builder >>> Builder.insert (SProxy :: SProxy l) a)

instance encodeRecordCons
    :: ( Cons l a r_ r
       , EncodeRecord r_ rl_
       , IsSymbol l
       , Encode_ a
       )
    => EncodeRecord r (Cons l a rl_)
  where
    encodeRecord_ _ opts rec =
      let obj = encodeRecord_ (RLProxy :: RLProxy rl_) opts (unsafeCoerce rec)
          l = reflectSymbol (SProxy :: SProxy l)
       in Object.insert (opts.fieldTransform l) (encode_ opts (Record.get (SProxy :: SProxy l) rec)) obj
