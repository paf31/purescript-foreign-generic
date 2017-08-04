module Data.Foreign.Generic.Class where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (mapExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readString, toForeign)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic.Types (Options, SumEncoding(..))
import Data.Foreign.Index (index)
import Data.Generic.Rep (Argument(..), Constructor(..), Field(..), NoArguments(..), NoConstructors, Product(..), Rec(..), Sum(..))
import Data.List (List(..), fromFoldable, null, singleton, toUnfoldable, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.StrMap as S
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))

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

class GenericDecodeFields a where
  decodeFields :: Options -> Foreign -> F a

class GenericEncodeFields a where
  encodeFields :: Options -> a -> S.StrMap Foreign

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
                 tag <- mapExcept (lmap (map (ErrorAtProperty contentsFieldName))) do
                   tag <- index f tagFieldName >>= readString
                   let expected = constructorTagTransform ctorName
                   unless (constructorTagTransform tag == expected) $
                     fail (ForeignError ("Expected " <> show expected <> " tag"))
                   pure tag
                 args <- mapExcept (lmap (map (ErrorAtProperty contentsFieldName)))
                           (index f contentsFieldName >>= readArguments)
                 pure (Constructor args)
    where
      ctorName = reflectSymbol (SProxy :: SProxy name)

      numArgs = countArgs (Proxy :: Proxy rep)

      readArguments args =
        case numArgs of
          Left a -> pure a
          Right 1 | opts.unwrapSingleArguments -> do
            { result, rest } <- decodeArgs opts 0 (singleton args)
            unless (null rest) $
              fail (ForeignError "Expected a single argument")
            pure result
          Right n -> do
            vals <- readArray args
            { result, rest } <- decodeArgs opts 0 (fromFoldable vals)
            unless (null rest) $
              fail (ForeignError ("Expected " <> show n <> " constructor arguments"))
            pure result

instance genericEncodeConstructor
  :: (IsSymbol name, GenericEncodeArgs rep)
  => GenericEncode (Constructor name rep) where
  encodeOpts opts (Constructor args) =
      if opts.unwrapSingleConstructors
        then maybe (toForeign {}) toForeign (encodeArgsArray args)
        else case opts.sumEncoding of
               TaggedObject { tagFieldName, contentsFieldName, constructorTagTransform } ->
                 toForeign (S.singleton tagFieldName (toForeign $ constructorTagTransform ctorName)
                           `S.union` maybe S.empty (S.singleton contentsFieldName) (encodeArgsArray args))
    where
      ctorName = reflectSymbol (SProxy :: SProxy name)

      encodeArgsArray :: rep -> Maybe Foreign
      encodeArgsArray = unwrapArguments <<< toUnfoldable <<< encodeArgs opts

      unwrapArguments :: Array Foreign -> Maybe Foreign
      unwrapArguments [] = Nothing
      unwrapArguments [x] | opts.unwrapSingleArguments = Just x
      unwrapArguments xs = Just (toForeign xs)

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
  :: Decode a
  => GenericDecodeArgs (Argument a) where
  decodeArgs _ i (x : xs) = do
    a <- mapExcept (lmap (map (ErrorAtIndex i))) (decode x)
    pure { result: Argument a, rest: xs, next: i + 1 }
  decodeArgs _ _ _ = fail (ForeignError "Not enough constructor arguments")

instance genericEncodeArgsArgument
  :: Encode a
  => GenericEncodeArgs (Argument a) where
  encodeArgs _ (Argument a) = singleton (encode a)

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

instance genericDecodeArgsRec
  :: GenericDecodeFields fields
  => GenericDecodeArgs (Rec fields) where
  decodeArgs opts i (x : xs) = do
    fields <- mapExcept (lmap (map (ErrorAtIndex i))) (decodeFields opts x)
    pure { result: Rec fields, rest: xs, next: i + 1 }
  decodeArgs _ _ _ = fail (ForeignError "Not enough constructor arguments")

instance genericEncodeArgsRec
  :: GenericEncodeFields fields
  => GenericEncodeArgs (Rec fields) where
  encodeArgs opts (Rec fs) = singleton (toForeign (encodeFields opts fs))

instance genericDecodeFieldsField
  :: (IsSymbol name, Decode a)
  => GenericDecodeFields (Field name a) where
  decodeFields opts x = do
    let name = opts.fieldTransform $ reflectSymbol (SProxy :: SProxy name)
    -- If `name` field doesn't exist, then `y` will be `undefined`.
    Field <$> (index x name >>= mapExcept (lmap (map (ErrorAtProperty name))) <<< decode)

instance genericEncodeFieldsField
  :: (IsSymbol name, Encode a)
  => GenericEncodeFields (Field name a) where
  encodeFields opts (Field a) =
    let name = opts.fieldTransform $ reflectSymbol (SProxy :: SProxy name)
    in S.singleton name (encode a)

instance genericDecodeFieldsProduct
  :: (GenericDecodeFields a, GenericDecodeFields b)
  => GenericDecodeFields (Product a b) where
  decodeFields opts x = Product <$> decodeFields opts x <*> decodeFields opts x

instance genericEncodeFieldsProduct
  :: (GenericEncodeFields a, GenericEncodeFields b)
  => GenericEncodeFields (Product a b) where
  encodeFields opts (Product a b) = encodeFields opts a `S.union` encodeFields opts b

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

instance genericCountArgsRec :: GenericCountArgs (Rec fields) where
  countArgs _ = Right 1
