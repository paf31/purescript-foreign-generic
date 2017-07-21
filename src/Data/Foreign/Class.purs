module Data.Foreign.Class where

import Prelude
import Data.String as S
import Data.Map as Map
import Data.StrMap as StrMap
import Control.Monad.Except (mapExcept)
import Data.Array ((..), zipWith, length)
import Data.Bifunctor (lmap)
import Data.Bitraversable (ltraverse)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readBoolean, readChar, readInt, readNumber, readString, toForeign)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), readNullOrUndefined, undefined)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Unsafe (char)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple)

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

instance arrayDecode :: Decode a => Decode (Array a) where
  decode = readArray >=> readElements where
    readElements :: Array Foreign -> F (Array a)
    readElements arr = sequence (zipWith readElement (0 .. length arr) arr)

    readElement :: Int -> Foreign -> F a
    readElement i value = mapExcept (lmap (map (ErrorAtIndex i))) (decode value)

instance strMapDecode :: Decode v => Decode (StrMap.StrMap v) where
  decode = sequence <<< StrMap.mapWithKey (\_ -> decode) <=< readStrMap


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

instance foreignEncode :: Encode Foreign where
  encode = id

instance stringEncode :: Encode String where
  encode = toForeign

instance charEncode :: Encode Char where
  encode = toForeign

instance booleanEncode :: Encode Boolean where
  encode = toForeign

instance numberEncode :: Encode Number where
  encode = toForeign

instance intEncode :: Encode Int where
  encode = toForeign

instance arrayEncode :: Encode a => Encode (Array a) where
  encode = toForeign <<< map encode

instance decodeNullOrUndefined :: Decode a => Decode (NullOrUndefined a) where
  decode = readNullOrUndefined decode

instance encodeNullOrUndefined :: Encode a => Encode (NullOrUndefined a) where
  encode (NullOrUndefined a) = maybe undefined encode a

instance strMapEncode :: Encode v => Encode (StrMap.StrMap v) where 
  encode = toForeign <<< StrMap.mapWithKey (\_ -> encode)


-- | The purpose of the `EncodeKey` class is to turn map keys into strings,
-- | so we can encode generic `Map`s as `StrMap`s.
class EncodeKey a where
  encodeKey :: a -> String

instance stringEncodeKey :: EncodeKey String where encodeKey = id
instance charEncodeKey :: EncodeKey Char where encodeKey = S.singleton
instance booleanEncodeKey :: EncodeKey Boolean where encodeKey = show
instance encodeKeyInt :: EncodeKey Int where encodeKey = show

-- | The purpose of the `DecodeKey` class is to turn strings into map keys,
-- | so we can decode generic `Map`s fram `StrMap`s.
class DecodeKey a where
  decodeKey :: String -> F a

instance stringDecodeKey :: DecodeKey String where
  decodeKey = pure <<< id
  
instance charDecodeKey :: DecodeKey Char where
  decodeKey s = case S.length s of
    1 -> pure $ char s
    _ -> fail $ ForeignError $ "invalid char key:" <> show s
    
instance booleanDecodeKey :: DecodeKey Boolean where
  decodeKey "true" = pure true
  decodeKey "false" = pure false
  decodeKey s = fail $ ForeignError $ "invalid boolean key:" <> show s
  
instance decodeKeyInt :: DecodeKey Int where
  decodeKey s = case fromString s of
    Nothing -> fail $ ForeignError $ "invalid key:" <> show s
    Just n -> pure n

instance mapEncode :: (EncodeKey k, Encode v, Ord k) => Encode (Map.Map k v) where
  encode = encode <<< StrMap.fromFoldable <<< encodeKeys <<< Map.toUnfoldable
    where
      encodeKeys :: Array (Tuple k v) -> Array (Tuple String v)
      encodeKeys = map $ lmap encodeKey

instance mapDecode :: (DecodeKey k, Decode v, Ord k) => Decode (Map.Map k v) where
  decode = pure <<< Map.fromFoldable <=< decodeKeys <<< StrMap.toUnfoldable <=< decode
    where
      decodeKeys :: Array (Tuple String v) -> F (Array (Tuple k v))
      decodeKeys = traverse $ ltraverse decodeKey 
