module Data.Foreign.Generic.Class where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (mapExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic.Types (Options, SumEncoding(..))
import Data.Foreign.JSON (addToObj)
import Data.Generic.Rep (Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..))
import Data.List (List(..), fromFoldable, null, singleton, toUnfoldable, (:))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (F, Foreign, ForeignError(..), fail, readArray, readString, unsafeToForeign)
import Foreign.Index (index)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (get)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
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

class GenericDecodeRowList (rl :: RowList) (from :: #Type) (to :: #Type) | rl -> from to where
  decodeRecord :: forall g . g rl -> Options -> Foreign -> F (Builder (Record from) (Record to))                                                    

class GenericEncodeArgs a where
  encodeArgs :: Options -> a -> List Foreign

class GenericEncodeRowList (rl :: RowList) (row :: #Type) | rl -> row where
  encodeRecord :: forall g . g rl -> Options -> Record row -> M.Map String Foreign

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
        then maybe (unsafeToForeign {}) unsafeToForeign (encodeArgsArray args)
        else case opts.sumEncoding of
               TaggedObject { tagFieldName, contentsFieldName, constructorTagTransform } ->
                 mapToObj (M.singleton tagFieldName (unsafeToForeign $ constructorTagTransform ctorName)
                           `M.union` maybe M.empty (M.singleton contentsFieldName) (encodeArgsArray args))
    where
      ctorName = reflectSymbol (SProxy :: SProxy name)

      encodeArgsArray :: rep -> Maybe Foreign
      encodeArgsArray = unwrapArguments <<< toUnfoldable <<< encodeArgs opts

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

instance genericDecodeArgsRecordArgument
  :: ( RowToList row rl
     , GenericDecodeRowList rl () row
  ) => GenericDecodeArgs (Argument (Record row)) where
  decodeArgs opts i (x:xs) = do
    steps <- decodeRecord rlp opts x
    let recArg = Argument $ Builder.build steps {}
    pure { result: recArg, rest: xs, next: i + 1 }
    where rlp :: RLProxy rl
          rlp = RLProxy
  decodeArgs _ _ _ = fail (ForeignError "Not enough constructor arguments")
    

else instance genericDecodeArgsArgument
  :: Decode a
  => GenericDecodeArgs (Argument a) where
  decodeArgs _ i (x : xs) = do
    a <- mapExcept (lmap (map (ErrorAtIndex i))) (decode x)
    pure { result: Argument a, rest: xs, next: i + 1 }
  decodeArgs _ _ _ = fail (ForeignError "Not enough constructor arguments")


mFail :: forall a . Int -> String -> Maybe a -> F a
mFail i err =
  maybe (fail (ErrorAtIndex i (ForeignError err))) pure


instance genericEncodeArgsRecordArgument
  :: ( RowToList row rl
     , GenericEncodeRowList rl row
  ) => GenericEncodeArgs (Argument (Record row)) where
  encodeArgs opts (Argument rec) =
    let fields = encodeRecord rlp opts rec
    in singleton (mapToObj fields)
    where rlp = RLProxy :: RLProxy rl 

else instance genericEncodeArgsArgument
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


instance genericDecodeRowListNil :: GenericDecodeRowList Nil () () where
  decodeRecord _ _ _ = pure identity

instance genericDecodeRowListCons
  :: ( Decode a
     , IsSymbol name
     , GenericDecodeRowList tail from from'
     , Row.Lacks name from'
     , Row.Cons name a from' to
  ) => GenericDecodeRowList (Cons name a tail) from to where
  decodeRecord _ opts x = do
    let fieldName = opts.fieldTransform $ reflectSymbol namep
    -- If `name` field doesn't exist, then `y` will be `undefined`.
    fieldValue <- index x name >>= mapExcept (lmap (map (ErrorAtProperty fieldName))) <<< decode
    rest <- decodeRecord tailp opts x
    let
      first :: Builder (Record from') (Record to)
      first = Builder.insert namep fieldValue
    pure $ first <<< rest
    where
      namep = SProxy :: SProxy name
      tailp = RLProxy :: RLProxy tail
      name = reflectSymbol namep


instance genericEncodeRowListNil :: GenericEncodeRowList Nil row where
  encodeRecord _ _ _ = M.empty

instance genericEncodeRowListCons
  :: ( Encode a
     , IsSymbol name
     , GenericEncodeRowList tail row
     , Row.Cons name a ignore row
  ) => GenericEncodeRowList (Cons name a tail) row where
  encodeRecord _ opts rec =
    let fieldName = opts.fieldTransform $ reflectSymbol namep
        fieldValue = encode value
        rest = encodeRecord tailp opts rec
    in 
      M.insert fieldName fieldValue rest
    where
      namep = SProxy :: SProxy name
      value = get namep rec
      tailp = RLProxy :: RLProxy tail

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


mapToObj :: M.Map String Foreign -> Foreign
mapToObj =
  foldlWithIndex (\field obj value -> addToObj field value obj) (unsafeToForeign {})