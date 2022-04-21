module Foreign.Generic.Class where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (except, mapExcept)
import Data.Array ((..), zipWith, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..))
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Foreign (F, Foreign, ForeignError(..), fail, readArray, readBoolean, readChar, readInt, readNumber, readString, unsafeToForeign)
import Foreign.Generic.Internal (readObject)
import Foreign.Index (index)
import Foreign.NullOrUndefined (readNullOrUndefined, null)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Encoding/Decoding options which can be used to customize
-- | `Decode` and `Encode` instances which are derived via
-- | `Generic` (see `genericEncode` and `genericDecode`).
type Options =
  { sumEncoding :: SumEncoding
  , unwrapSingleConstructors :: Boolean
  , unwrapSingleArguments :: Boolean
  , fieldTransform :: String -> String
  }

-- | The encoding of sum types for your type.
-- | `TaggedObject`s will be encoded in the form `{ [tagFieldName]: "ConstructorTag", [contentsFieldName]: "Contents"}`.
-- | `constructorTagTransform` can be provided to transform the constructor tag to a form you use, e.g. `toLower`/`toUpper`.
data SumEncoding
  = TaggedObject
    { tagFieldName :: String
    , contentsFieldName :: String
    , constructorTagTransform :: String -> String
    }

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
  decode = decodeWithOptions defaultOptions

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
  encode = encodeWithOptions defaultOptions

-- | When deriving `En`/`Decode` instances using `Generic`, we want
-- | the `Options` object to apply to the outermost record type(s)
-- | under the data constructors.
-- |
-- | For this reason, we cannot use `En`/`Decode` directly when we
-- | reach an `Argument` during generic traversal of a type, because it
-- | might be a record type. Instead, we need to peel off any record
-- | type(s) and apply the appropriate `Options` before we can delegate
-- | to `En`/`Decode`, which can bake in its own `Options`.
class DecodeWithOptions a where
  decodeWithOptions :: Options -> Foreign -> F a

-- | See the comment on `DecodeWithOptions`.
class EncodeWithOptions a where
  encodeWithOptions :: Options -> a -> Foreign

instance decodeWithOptionsRecord :: (RowToList r rl, DecodeRecord r rl) => DecodeWithOptions (Record r) where
  decodeWithOptions opts = map (flip Builder.build {}) <$> decodeRecordWithOptions (Proxy :: Proxy rl) opts
else instance decodeWithOptionsOther :: Decode a => DecodeWithOptions a where
  decodeWithOptions _ = decode

instance encodeWithOptionsRecord :: (RowToList r rl, EncodeRecord r rl) => EncodeWithOptions (Record r) where
  encodeWithOptions opts = unsafeToForeign <<< encodeRecordWithOptions (Proxy :: Proxy rl) opts
else instance encodeWithOptionsOther :: Encode a => EncodeWithOptions a where
  encodeWithOptions _ = encode

class DecodeRecord r rl | rl -> r where
  decodeRecordWithOptions :: Proxy rl -> Options -> Foreign -> F (Builder {} (Record r))

class EncodeRecord r rl | rl -> r where
  encodeRecordWithOptions :: Proxy rl -> Options -> Record r -> Object Foreign

instance decodeRecordNil :: DecodeRecord () Nil where
  decodeRecordWithOptions _ _ _ = pure identity

instance encodeRecordNil :: EncodeRecord () Nil where
  encodeRecordWithOptions _ _ _ = Object.empty

instance decodeRecordCons
    :: ( Cons l a r_ r
       , DecodeRecord r_ rl_
       , IsSymbol l
       , DecodeWithOptions a
       , Lacks l r_
       )
    => DecodeRecord r (Cons l a rl_)
  where
    decodeRecordWithOptions _ opts f = do
      builder <- decodeRecordWithOptions (Proxy :: Proxy rl_) opts f
      let l = reflectSymbol (Proxy :: Proxy l)
          l_transformed = (opts.fieldTransform l)
      f_ <- index f l_transformed
      a <- mapExcept (lmap (map (ErrorAtProperty l_transformed))) (decodeWithOptions opts f_)
      pure (builder >>> Builder.insert (Proxy :: Proxy l) a)

instance encodeRecordCons
    :: ( Cons l a r_ r
       , EncodeRecord r_ rl_
       , IsSymbol l
       , EncodeWithOptions a
       )
    => EncodeRecord r (Cons l a rl_)
  where
    encodeRecordWithOptions _ opts rec =
      let obj = encodeRecordWithOptions (Proxy :: Proxy rl_) opts (unsafeCoerce rec)
          l = reflectSymbol (Proxy :: Proxy l)
       in Object.insert (opts.fieldTransform l) (encodeWithOptions opts (Record.get (Proxy :: Proxy l) rec)) obj

class GenericDecode a where
  decodeOpts :: Options -> Foreign -> F a

class GenericEncode a where
  encodeOpts :: Options -> a -> Foreign

class GenericDecodeArgs a where
  decodeArgs :: Options -> Int -> List Foreign -> F { result :: a
                                                    , rest :: List Foreign
                                                    , next :: Int
                                                    }

class GenericEncodeArgs a where
  encodeArgs :: Options -> a -> List Foreign

class GenericCountArgs a where
  countArgs :: Proxy a -> Either a Int

instance genericDecodeNoConstructors :: GenericDecode NoConstructors where
  decodeOpts opts _ = fail (ForeignError "No constructors")

instance genericEncodeNoConstructors :: GenericEncode NoConstructors where
  encodeOpts opts a = encodeOpts opts a

instance genericDecodeConstructor
  :: (IsSymbol name, GenericDecodeArgs rep, GenericCountArgs rep)
  => GenericDecode (Constructor name rep) where
  decodeOpts opts f =
      if opts.unwrapSingleConstructors
        then Constructor <$> readArguments f
        else case opts.sumEncoding of
               TaggedObject { tagFieldName, contentsFieldName, constructorTagTransform } -> do
                 tag <- mapExcept (lmap (map (ErrorAtProperty tagFieldName))) do
                   tag <- index f tagFieldName >>= readString
                   let expected = constructorTagTransform ctorName
                   unless (tag == expected) $
                     fail (ForeignError ("Expected " <> show expected <> " tag"))
                   pure tag
                 args <- mapExcept (lmap (map (ErrorAtProperty contentsFieldName)))
                           (index f contentsFieldName >>= readArguments)
                 pure (Constructor args)
    where
      ctorName = reflectSymbol (Proxy :: Proxy name)

      numArgs = countArgs (Proxy :: Proxy rep)

      readArguments args =
        case numArgs of
          Left a -> pure a
          Right 1 | opts.unwrapSingleArguments -> do
            { result, rest } <- decodeArgs opts 0 (List.singleton args)
            unless (List.null rest) $
              fail (ForeignError "Expected a single argument")
            pure result
          Right n -> do
            vals <- readArray args
            { result, rest } <- decodeArgs opts 0 (List.fromFoldable vals)
            unless (List.null rest) $
              fail (ForeignError ("Expected " <> show n <> " constructor arguments"))
            pure result

instance genericEncodeConstructor
  :: (IsSymbol name, GenericEncodeArgs rep)
  => GenericEncode (Constructor name rep) where
  encodeOpts opts (Constructor args) =
      if opts.unwrapSingleConstructors
        then maybe (unsafeToForeign {}) unsafeToForeign (encodeArgsArray args)
        else case opts.sumEncoding of
               TaggedObject { tagFieldName, contentsFieldName, constructorTagTransform } ->
                 unsafeToForeign (Object.singleton tagFieldName (unsafeToForeign $ constructorTagTransform ctorName)
                           `Object.union` maybe Object.empty (Object.singleton contentsFieldName) (encodeArgsArray args))
    where
      ctorName = reflectSymbol (Proxy :: Proxy name)

      encodeArgsArray :: rep -> Maybe Foreign
      encodeArgsArray = unwrapArguments <<< List.toUnfoldable <<< encodeArgs opts

      unwrapArguments :: Array Foreign -> Maybe Foreign
      unwrapArguments [] = Nothing
      unwrapArguments [x] | opts.unwrapSingleArguments = Just x
      unwrapArguments xs = Just (unsafeToForeign xs)

instance genericDecodeSum
  :: (GenericDecode a, GenericDecode b)
  => GenericDecode (Sum a b) where
  decodeOpts opts f = Inl <$> decodeOpts opts' f <|> Inr <$> decodeOpts opts' f
    where
      -- Reuse the unwrapSingleConstructors flag, since we cannot have a single
      -- constructor at this point anyway.
      opts' = opts { unwrapSingleConstructors = false }

instance genericEncodeSum
  :: (GenericEncode a, GenericEncode b)
  => GenericEncode (Sum a b) where
  encodeOpts opts (Inl a) = encodeOpts (opts { unwrapSingleConstructors = false }) a
  encodeOpts opts (Inr b) = encodeOpts (opts { unwrapSingleConstructors = false }) b

instance genericDecodeArgsNoArguments :: GenericDecodeArgs NoArguments where
  decodeArgs _ i Nil = pure { result: NoArguments, rest: Nil, next: i }
  decodeArgs _ _ _ = fail (ForeignError "Too many constructor arguments")

instance genericEncodeArgsNoArguments :: GenericEncodeArgs NoArguments where
  encodeArgs _ = mempty

instance genericDecodeArgsArgument
  :: DecodeWithOptions a
  => GenericDecodeArgs (Argument a) where
  decodeArgs opts i (x : xs) = do
    a <- mapExcept (lmap (map (ErrorAtIndex i))) (decodeWithOptions opts x)
    pure { result: Argument a, rest: xs, next: i + 1 }
  decodeArgs _ _ _ = fail (ForeignError "Not enough constructor arguments")

instance genericEncodeArgsArgument
  :: EncodeWithOptions a
  => GenericEncodeArgs (Argument a) where
  encodeArgs opts (Argument a) = List.singleton (encodeWithOptions opts a)

instance genericDecodeArgsProduct
  :: (GenericDecodeArgs a, GenericDecodeArgs b)
  => GenericDecodeArgs (Product a b) where
  decodeArgs opts i xs = do
    { result: resA, rest: xs1, next: i1 } <- decodeArgs opts i xs
    { result: resB, rest, next } <- decodeArgs opts i1 xs1
    pure { result: Product resA resB, rest, next }

instance genericEncodeArgsProduct
  :: (GenericEncodeArgs a, GenericEncodeArgs b)
  => GenericEncodeArgs (Product a b) where
  encodeArgs opts (Product a b) = encodeArgs opts a <> encodeArgs opts b

instance genericCountArgsNoArguments :: GenericCountArgs NoArguments where
  countArgs _ = Left NoArguments

instance genericCountArgsArgument :: GenericCountArgs (Argument a) where
  countArgs _ = Right 1

instance genericCountArgsProduct
  :: (GenericCountArgs a, GenericCountArgs b)
  => GenericCountArgs (Product a b) where
  countArgs _ =
    case countArgs (Proxy :: Proxy a), countArgs (Proxy :: Proxy b) of
      Left a , Left b  -> Left (Product a b)
      Left _ , Right n -> Right n
      Right n, Left _  -> Right n
      Right n, Right m -> Right (n + m)
