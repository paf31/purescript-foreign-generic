module Data.Foreign.Class where

import Prelude
import Control.Monad.Except (except, mapExcept)
import Data.Array ((..), zipWith, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), readArray, readBoolean, readChar, readInt, readNumber, readString, toForeign)
import Data.Foreign.NullOrUndefined (readNullOrUndefined, undefined)
import Data.Maybe (Maybe, maybe)
import Data.StrMap as StrMap
import Data.Traversable (sequence)
import Data.Foreign.Internal (readStrMap)

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

instance arrayDecode :: Decode a => Decode (Array a) where
  decode = readArray >=> readElements where
    readElements :: Array Foreign -> F (Array a)
    readElements arr = sequence (zipWith readElement (0 .. length arr) arr)

    readElement :: Int -> Foreign -> F a
    readElement i value = mapExcept (lmap (map (ErrorAtIndex i))) (decode value)

instance maybeDecode :: Decode a => Decode (Maybe a) where
  decode = readNullOrUndefined decode

instance strMapDecode :: (Decode v) => Decode (StrMap.StrMap v) where
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

instance voidEncode :: Encode Void where
  encode = absurd

instance unitEncode :: Encode Unit where
  encode _ = toForeign {}

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

instance maybeEncode :: Encode a => Encode (Maybe a) where
  encode = maybe undefined encode

instance strMapEncode :: Encode v => Encode (StrMap.StrMap v) where
  encode = toForeign <<< StrMap.mapWithKey (\_ -> encode)
