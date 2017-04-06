module Data.Foreign.Generic
  ( parseJSON
  , defaultOptions
  , readGeneric
  , toForeignGeneric
  , readJSONGeneric
  , toJSONGeneric
  ) where

import Prelude
import Control.Monad.Eff (runPure)
import Control.Monad.Eff.Exception (EXCEPTION, message, try)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Control.Monad.Except (ExceptT(..))
import Data.Bifunctor (lmap)
import Data.Foreign (F, Foreign, ForeignError(..))
import Data.Foreign.Generic.Classes (class GenericDecode, class GenericEncode, decodeOpts, encodeOpts)
import Data.Foreign.Generic.Types (Options, SumEncoding(..))
import Data.Generic.Rep (class Generic, from, to)
import Data.Identity (Identity(..))
import Global.Unsafe (unsafeStringify)

foreign import parseJSONImpl :: forall eff. EffFn1 (exception :: EXCEPTION | eff) String Foreign

parseJSON :: String -> F Foreign
parseJSON =
  ExceptT
  <<< Identity
  <<< lmap (pure <<< JSONError <<< message)
  <<< runPure
  <<< try
  <<< runEffFn1 parseJSONImpl

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
readGeneric
  :: forall a rep
   . Generic a rep
  => GenericDecode rep
  => Options
  -> Foreign
  -> F a
readGeneric opts = map to <<< decodeOpts opts

-- | Generate a `Foreign` value compatible with the `readGeneric` function.
toForeignGeneric
  :: forall a rep
   . Generic a rep
  => GenericEncode rep
  => Options
  -> a
  -> Foreign
toForeignGeneric opts = encodeOpts opts <<< from

-- | Read a value which has a `Generic` type from a JSON String
readJSONGeneric
  :: forall a rep
   . Generic a rep
  => GenericDecode rep
  => Options
  -> String
  -> F a
readJSONGeneric opts = parseJSON >=> readGeneric opts

-- | Write a value which has a `Generic` type as a JSON String
toJSONGeneric
  :: forall a rep
   . Generic a rep
  => GenericEncode rep
  => Options
  -> a
  -> String
toJSONGeneric opts = toForeignGeneric opts >>> unsafeStringify
