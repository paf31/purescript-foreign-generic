module Data.Foreign.Generic.EnumEncoding where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readString, toForeign)
import Data.Generic.Rep (class Generic, Argument, Constructor(Constructor), NoArguments(NoArguments), Product, Rec, Sum(Inr, Inl), from, to)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Partial.Unsafe (unsafeCrashWith)

type GenericEnumOptions =
  { constructorTagTransform :: String -> String
  }

defaultGenericEnumOptions :: GenericEnumOptions
defaultGenericEnumOptions =
  { constructorTagTransform: id
  }

-- | A generic function to be used with "Enums", or sum types with only no-argument constructors. This is used for decoding from strings to one of the constructors, combined with the `constructorTagTransform` property of `SumEncoding`.
genericDecodeEnum
  :: forall a rep
   . Generic a rep
  => GenericDecodeEnum rep
  => GenericEnumOptions
  -> Foreign
  -> F a
genericDecodeEnum opts = map to <<< decodeEnum opts

-- | A generic function to be used with "Enums", or sum types with only no-argument constructors. This is used for encoding to strings from one of the constructors, combined with the `constructorTagTransform` property of `SumEncoding`.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Fruit = Apple | Banana | Frikandel
-- | derive instance geFruit :: Generic Fruit _
-- | instance eFruit :: Encode Fruit where
-- |   encode = genericEncodeEnum defaultGenericEnumOptions
genericEncodeEnum
  :: forall a rep
   . Generic a rep
  => GenericEncodeEnum rep
  => GenericEnumOptions
  -> a
  -> Foreign
genericEncodeEnum opts = encodeEnum opts <<< from

-- | A type class for type representations that can be used for decoding to an Enum. Only the sum and no-argument constructor instances are valid, while others provide a `Fail` constraint to fail in compilation.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Fruit = Apple | Banana | Frikandel
-- | derive instance geFruit :: Generic Fruit _
-- | instance dFruit :: Decode Fruit where
-- |   decode = genericDecodeEnum defaultGenericEnumOptions
-- | ```
class GenericDecodeEnum a where
  decodeEnum :: GenericEnumOptions -> Foreign -> F a

-- | A type class for type representations that can be used for encoding from an Enum. Only the sum and no-argument constructor instances are valid, while others provide a `Fail` constraint to fail in compilation.
class GenericEncodeEnum a where
  encodeEnum :: GenericEnumOptions -> a -> Foreign

instance sumGenericDecodeEnum
  :: (GenericDecodeEnum a, GenericDecodeEnum b)
  => GenericDecodeEnum (Sum a b) where
  decodeEnum opts f = Inl <$> decodeEnum opts f <|> Inr <$> decodeEnum opts f

instance ctorNoArgsGenericDecodeEnum
  :: IsSymbol name
  => GenericDecodeEnum (Constructor name NoArguments) where
  decodeEnum {constructorTagTransform} f = do
    tag <- readString f
    unless (tag == ctorName) $
      fail (ForeignError ("Expected " <> show ctorName <> " tag for unary constructor literal " <> ctorName))
    pure $ Constructor NoArguments
    where
      ctorName = constructorTagTransform $ reflectSymbol (SProxy :: SProxy name)

instance ctorArgumentGenericDecodeEnum
  :: Fail "genericEncode/DecodeEnum cannot be used on types that are not sums of constructors with no arguments."
  => GenericDecodeEnum (Constructor name (Argument a)) where
  decodeEnum _ _ = unsafeCrashWith "unreachable decodeEnum was reached."

instance ctorProductGenericDecodeEnum
  :: Fail "genericEncode/DecodeEnum cannot be used on types that are not sums of constructors with no arguments."
  => GenericDecodeEnum (Constructor name (Product a b)) where
  decodeEnum _ _ = unsafeCrashWith "unreachable decodeEnum was reached."

instance ctorRecGenericDecodeEnum
  :: Fail "genericEncode/DecodeEnum cannot be used on types that are not sums of constructors with no arguments."
  => GenericDecodeEnum (Constructor name (Rec a)) where
  decodeEnum _ _ = unsafeCrashWith "unreachable decodeEnum was reached."

instance sumGenericEncodeEnum
  :: (GenericEncodeEnum a, GenericEncodeEnum b)
  => GenericEncodeEnum (Sum a b) where
  encodeEnum opts (Inl a) = encodeEnum opts a
  encodeEnum opts (Inr b) = encodeEnum opts b

instance ctorNoArgsGenericEncodeEnum
  :: IsSymbol name
  => GenericEncodeEnum (Constructor name NoArguments) where
  encodeEnum {constructorTagTransform} _ = toForeign ctorName
    where
      ctorName = constructorTagTransform $ reflectSymbol (SProxy :: SProxy name)

instance ctorArgumentGenericEncodeEnum
  :: Fail "genericEncode/DecodeEnum cannot be used on types that are not sums of constructors with no arguments."
  => GenericEncodeEnum (Constructor name (Argument a)) where
  encodeEnum _ _ = unsafeCrashWith "unreachable encodeEnum was reached."

instance ctorProductGenericEncodeEnum
  :: Fail "genericEncode/DecodeEnum cannot be used on types that are not sums of constructors with no arguments."
  => GenericEncodeEnum (Constructor name (Product a b)) where
  encodeEnum _ _ = unsafeCrashWith "unreachable encodeEnum was reached."

instance ctorRecGenericEncodeEnum
  :: Fail "genericEncode/DecodeEnum cannot be used on types that are not sums of constructors with no arguments."
  => GenericEncodeEnum (Constructor name (Rec a)) where
  encodeEnum _ _ = unsafeCrashWith "unreachable encodeEnum was reached."
