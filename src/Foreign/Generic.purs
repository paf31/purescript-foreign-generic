module Foreign.Generic
  ( genericDecode
  , genericEncode
  , decodeJSON
  , encodeJSON
  , genericDecodeJSON
  , genericEncodeJSON
  , module Reexports
  ) where

import Prelude

import Data.Generic.Rep (class Generic, from, to)
import Foreign (F, Foreign)
import Foreign (F, Foreign, ForeignError(..)) as Reexports
import Foreign.Generic.Class (class Decode, class Encode, class GenericDecode, class GenericEncode, Options, decode, decodeOpts, encode, encodeOpts)
import Foreign.Generic.Class (class Decode, class Encode, class GenericDecode, class GenericEncode, Options, SumEncoding(..), defaultOptions, decode, encode) as Reexports
import Foreign.JSON (decodeJSONWith, parseJSON)
import Global.Unsafe (unsafeStringify)

-- | Read a value which has a `Generic` type.
genericDecode
  :: forall a rep
   . Generic a rep
  => GenericDecode rep
  => Options
  -> Foreign
  -> F a
genericDecode opts = map to <<< decodeOpts opts

-- | Generate a `Foreign` value compatible with the `genericDecode` function.
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

-- | Encode a JSON string using an `Encode` instance.
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
