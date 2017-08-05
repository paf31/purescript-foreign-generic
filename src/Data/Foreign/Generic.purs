module Data.Foreign.Generic
  ( defaultOptions
  , genericDecode
  , genericEncode
  , decodeJSON
  , encodeJSON
  , genericDecodeJSON
  , genericEncodeJSON
  ) where

import Prelude

import Data.Foreign (F, Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode, decodeOpts, encodeOpts)
import Data.Foreign.Generic.Types (Options, SumEncoding(..))
import Data.Foreign.JSON (parseJSON, decodeJSONWith)
import Data.Generic.Rep (class Generic, from, to)
import Global.Unsafe (unsafeStringify)

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
        , constructorTagTransform: id
        }
  , unwrapSingleConstructors: false
  , unwrapSingleArguments: true
  , fieldTransform: id
  }

-- | Read a value which has a `Generic` type.
genericDecode
  :: forall a rep
   . Generic a rep
  => GenericDecode rep
  => Options
  -> Foreign
  -> F a
genericDecode opts = map to <<< decodeOpts opts

-- | Generate a `Foreign` value compatible with the `readGeneric` function.
genericEncode
  :: forall a rep
   . Generic a rep
  => GenericEncode rep
  => Options
  -> a
  -> Foreign
genericEncode opts = encodeOpts opts <<< from

-- | Decode a JSON string using a `Decode` instance.
decodeJSON
  :: forall a
   . Decode a
  => String
  -> F a
decodeJSON = decodeJSONWith decode

-- | Decode a JSON string using a `Decode` instance.
encodeJSON
  :: forall a
   . Encode a
  => a
  -> String
encodeJSON = unsafeStringify <<< encode

-- | Read a value which has a `Generic` type from a JSON String
genericDecodeJSON
  :: forall a rep
   . Generic a rep
  => GenericDecode rep
  => Options
  -> String
  -> F a
genericDecodeJSON opts = genericDecode opts <=< parseJSON

-- | Write a value which has a `Generic` type as a JSON String
genericEncodeJSON
  :: forall a rep
   . Generic a rep
  => GenericEncode rep
  => Options
  -> a
  -> String
genericEncodeJSON opts = unsafeStringify <<< genericEncode opts
