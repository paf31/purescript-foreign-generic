module Data.Foreign.Generic where

import Prelude
import Data.Foreign (F, Foreign, parseJSON)
import Data.Foreign.Generic.Classes (class GenericDecode, class GenericEncode, decodeOpts, encodeOpts)
import Data.Foreign.Generic.Types (Options, SumEncoding(..))
import Data.Generic.Rep (class Generic, from, to)
import Global.Unsafe (unsafeStringify)

defaultOptions :: Options
defaultOptions =
  { sumEncoding:
      TaggedObject
        { tagFieldName: "tag"
        , contentsFieldName: "contents"
        }
  , unwrapSingleConstructors: false
  , unwrapSingleArguments: true
  }

-- | Read a value which has a `Generic` type.
readGeneric :: forall a rep. Generic a rep => GenericDecode rep => Options -> Foreign -> F a
readGeneric opts = map to <<< decodeOpts opts

-- | Generate a `Foreign` value compatible with the `readGeneric` function.
toForeignGeneric :: forall a rep. Generic a rep => GenericEncode rep => Options -> a -> Foreign
toForeignGeneric opts = encodeOpts opts <<< from

-- | Read a value which has a `Generic` type from a JSON String
readJSONGeneric :: forall a rep. Generic a rep => GenericDecode rep => Options -> String -> F a
readJSONGeneric opts = parseJSON >=> readGeneric opts

-- | Write a value which has a `Generic` type as a JSON String
toJSONGeneric :: forall a rep. Generic a rep => GenericEncode rep => Options -> a -> String
toJSONGeneric opts = toForeignGeneric opts >>> unsafeStringify
