module Foreign.Generic.Class where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (except, mapExcept)
import Data.Array ((..), zipWith, length)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Generic.Rep (Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..))
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Foreign (F, Foreign, ForeignError(..), fail, typeOf, isArray, readArray, readBoolean, readChar, readInt, readNull, readNumber, readString, unsafeToForeign, unsafeFromForeign)
import Foreign.Generic.Internal (readObject)
import Foreign.Index (hasProperty, index, readProp)
import Foreign.Keys as Keys
import Foreign.NullOrUndefined (readNullOrUndefined, null)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
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
    , unwrapRecords :: Boolean
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
        , unwrapRecords: false
        }
  , unwrapSingleConstructors: false
  , unwrapSingleArguments: true
  , fieldTransform: identity
  }

-- | Aeson unwraps records, use this sum encoding with Aeson generated json
aesonSumEncoding :: SumEncoding
aesonSumEncoding = TaggedObject
        { tagFieldName: "tag"
        , contentsFieldName: "contents"
        , constructorTagTransform: identity
        , unwrapRecords: true
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

instance listDecode :: Decode a => Decode (List a) where
  decode f = let (array :: F (Array a)) = decode f in List.fromFoldable <$> array

instance tupleDecode :: (Decode a, Decode b) => Decode (Tuple a b) where
  decode f = do
    (arr :: Array Foreign) <- decode f
    case arr of
      [a, b] -> Tuple <$> decode a <*> decode b
      _ -> except (Left (pure (ForeignError "Decode: Tuple was not a list of exactly 2 items")))

instance maybeDecode :: Decode a => Decode (Maybe a) where
  decode = readNullOrUndefined decode

instance objectDecode :: Decode v => Decode (Object v) where
  decode = sequence <<< Object.mapWithKey (\_ -> decode) <=< readObject

instance recordDecode :: (RowToList r rl, DecodeRecord r rl) => Decode (Record r) where
  decode = decodeWithOptions defaultOptions

instance mapDecode :: (Ord k, Decode k, Decode v) => Decode (Map k v) where
  decode json =  decodeAsArrayOfPairs json <|> decodeAsObjectWithStringKeys json <|> decodeAsNull json
    where
      decodeAsArrayOfPairs o = do
        pairs <- readArray o
        asArray <-
          traverse
            ( \foreignPair ->
                readArray foreignPair
                  >>= case _ of
                      [ foreignKey, foreignValue ] -> Tuple <$> decode foreignKey <*> decode foreignValue
                      other -> fail $ TypeMismatch "Array (key-value pair)" "<Foreign>"
            )
            pairs
        pure $ Map.fromFoldable asArray

      decodeAsObjectWithStringKeys o = do
        keys <- Keys.keys o
        asArray <-
          traverse
            ( \keyString -> do
                foreignValue <- readProp keyString o
                key <- decode $ encode keyString
                value <- decode foreignValue
                pure (Tuple key value)
            )
            keys
        pure $ Map.fromFoldable asArray

      decodeAsNull o = do
        _ <- readNull o
        pure mempty


instance setDecode :: (Ord a, Decode a) => Decode (Set a) where
  decode f = do
    (arr :: Array a) <- decode f
    pure $ Set.fromFoldable arr

instance eitherDecode :: (Decode a, Decode b) => Decode (Either a b) where
  decode value =
      (readProp "Left" value >>= (map Left <<< decode))
      <|>
      (readProp "Right" value >>= (map Right <<< decode))

instance bigIntDecode :: Decode BigInt where
  decode value = do
    str <- readString value
    except $ note (pure (ForeignError ("Expected BigInt"))) $ BigInt.fromString str

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

instance listEncode :: Encode a => Encode (List a) where
  encode f = let (arr :: Array a) = List.toUnfoldable f in encode arr

instance encodeTuple :: (Encode a, Encode b) => Encode (Tuple a b) where
  encode (Tuple a b) = unsafeToForeign [encode a, encode b]

instance maybeEncode :: Encode a => Encode (Maybe a) where
  encode = maybe null encode

instance objectEncode :: Encode v => Encode (Object v) where
  encode = unsafeToForeign <<< Object.mapWithKey (\_ -> encode)

instance recordEncode :: (RowToList r rl, EncodeRecord r rl) => Encode (Record r) where
  encode = encodeWithOptions defaultOptions

instance mapEncode :: (Encode k, Encode v) => Encode (Map k v) where
  encode m = encode (Map.toUnfoldable m :: Array _)

instance setEncode :: (Ord a, Encode a) => Encode (Set a) where
  encode s = let (arr :: Array a) = Set.toUnfoldable s in encode arr


instance encodeEither :: (Encode a, Encode b) => Encode (Either a b) where
  encode (Left a) = encode $ Object.singleton "Left" a
  encode (Right b) = encode $ Object.singleton "Right" b

instance bigIntEncode :: Encode BigInt where
  encode = encode <<< BigInt.toString

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
  decodeWithOptions opts = map (flip Builder.build {}) <$> decodeRecordWithOptions (RLProxy :: RLProxy rl) opts
else instance decodeWithOptionsOther :: Decode a => DecodeWithOptions a where
  decodeWithOptions _ = decode

instance encodeWithOptionsRecord :: (RowToList r rl, EncodeRecord r rl) => EncodeWithOptions (Record r) where
  encodeWithOptions opts = unsafeToForeign <<< encodeRecordWithOptions (RLProxy :: RLProxy rl) opts
else instance encodeWithOptionsOther :: Encode a => EncodeWithOptions a where
  encodeWithOptions _ = encode

class DecodeRecord r rl | rl -> r where
  decodeRecordWithOptions :: RLProxy rl -> Options -> Foreign -> F (Builder {} (Record r))

class EncodeRecord r rl | rl -> r where
  encodeRecordWithOptions :: RLProxy rl -> Options -> Record r -> Object Foreign

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
      builder <- decodeRecordWithOptions (RLProxy :: RLProxy rl_) opts f
      let l = reflectSymbol (SProxy :: SProxy l)
          l_transformed = (opts.fieldTransform l)
      f_ <- index f l_transformed
      a <- mapExcept (lmap (map (ErrorAtProperty l_transformed))) (decodeWithOptions opts f_)
      pure (builder >>> Builder.insert (SProxy :: SProxy l) a)

instance encodeRecordCons
    :: ( Cons l a r_ r
       , EncodeRecord r_ rl_
       , IsSymbol l
       , EncodeWithOptions a
       )
    => EncodeRecord r (Cons l a rl_)
  where
    encodeRecordWithOptions _ opts rec =
      let obj = encodeRecordWithOptions (RLProxy :: RLProxy rl_) opts (unsafeCoerce rec)
          l = reflectSymbol (SProxy :: SProxy l)
       in Object.insert (opts.fieldTransform l) (encodeWithOptions opts (Record.get (SProxy :: SProxy l) rec)) obj

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
               TaggedObject { tagFieldName, contentsFieldName, constructorTagTransform, unwrapRecords } -> do
                 tag <- mapExcept (lmap (map (ErrorAtProperty tagFieldName))) do
                   tag <- index f tagFieldName >>= readString
                   let expected = constructorTagTransform ctorName
                   unless (tag == expected) $
                     fail (ForeignError ("Expected " <> show expected <> " tag"))
                   pure tag
                 args <- mapExcept (lmap (map (ErrorAtProperty contentsFieldName)))
                           ((contents unwrapRecords contentsFieldName f) >>= readArguments)
                 pure (Constructor args)
    where
      ctorName = reflectSymbol (SProxy :: SProxy name)

      numArgs = countArgs (Proxy :: Proxy rep)

      contents :: Boolean -> String -> Foreign -> F Foreign
      contents unwrapRecords contentsFieldName f'
        | unwrapRecords && not (hasProperty contentsFieldName f') = pure f'
        | otherwise = index f' contentsFieldName

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
                           `Object.union` objectFromArgs opts.sumEncoding (encodeArgsArray args))
    where
      ctorName = reflectSymbol (SProxy :: SProxy name)

      objectFromArgs :: SumEncoding -> Maybe Foreign -> Object Foreign
      objectFromArgs _ Nothing = Object.empty
      objectFromArgs (TaggedObject { contentsFieldName, unwrapRecords }) (Just f)
        | typeOf f == "object" && not isArray f && unwrapRecords = unsafeFromForeign f
        | otherwise = Object.singleton contentsFieldName f

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
