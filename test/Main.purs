module Test.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (toLower, toUpper)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign (isNull, unsafeToForeign)
import Foreign.Generic (class Decode, class Encode, class GenericDecode, class GenericEncode, Options, decode, encode, defaultOptions, decodeJSON, encodeJSON, genericDecodeJSON, genericEncodeJSON)
import Foreign.Generic.EnumEncoding (class GenericDecodeEnum, class GenericEncodeEnum, GenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Foreign.Index (readProp)
import Foreign.JSON (parseJSON)
import Foreign.Object as Object
import Global.Unsafe (unsafeStringify)
import Test.Assert (assert, assert')
import Test.Types (Fruit(..), IntList(..), RecordTest(..), Tree(..), TupleArray(..), UndefinedTest(..))

buildTree :: forall a. (a -> TupleArray a a) -> Int -> a -> Tree a
buildTree _ 0 a = Leaf a
buildTree f n a = Branch $ buildTree (bimap f f) (n - 1) (f a)

-- A balanced binary tree of depth N
makeTree :: Int -> Tree Int
makeTree n = buildTree (\i -> TupleArray (Tuple (2 * i) (2 * i + 1))) n 0

throw :: String -> Effect Unit
throw = flip assert' false

testRoundTrip
  :: ∀ a
   . Eq a
  => Decode a
  => Encode a
  => a
  -> Effect Unit
testRoundTrip x = do
  let json = encodeJSON x
  log json
  case runExcept (decodeJSON json) of
    Right y -> assert (x == y)
    Left err -> throw (show err)

testGenericRoundTrip
  :: ∀ a r
   . Eq a
  => Generic a r
  => GenericDecode r
  => GenericEncode r
  => Options
  -> a
  -> Effect Unit
testGenericRoundTrip opts x = do
  let json = genericEncodeJSON opts x
  log json
  case runExcept (genericDecodeJSON opts json) of
    Right y -> assert (x == y)
    Left err -> throw (show err)

testOption
  :: ∀ a rep
   . Eq a
  => Generic a rep
  => GenericEncodeEnum rep
  => GenericDecodeEnum rep
  => GenericEnumOptions
  -> String
  -> a
  -> Effect Unit
testOption options string value = do
  let json = unsafeStringify $ genericEncodeEnum options value
  log json
  case runExcept $ Tuple <$> decode' json <*> decode' string of
    Right (Tuple x y) -> assert (value == y && value == x)
    Left err -> throw (show err)
  where
    decode' = genericDecodeEnum options <=< parseJSON

testUnaryConstructorLiteral :: Effect Unit
testUnaryConstructorLiteral = do
    testOption (makeCasingOptions toUpper) "\"FRIKANDEL\"" Frikandel
    testOption (makeCasingOptions toLower) "\"frikandel\"" Frikandel
  where
    makeCasingOptions f =
      { constructorTagTransform: f
      }

-- Test that `Nothing` record fields, when encoded to JSON, are present and
-- encoded as `null`
testNothingToNull :: Effect Unit
testNothingToNull =
  let
    json = encode (UndefinedTest {a: Nothing})
  in do
    log (encodeJSON json)
    case runExcept (pure json >>= readProp "contents" >>= readProp "a") of
      Right val ->
        when (not (isNull val))
          (throw ("property 'a' was not null; got: " <> encodeJSON val))
      Left err ->
        throw (show err)

-- Test that `Maybe` fields which are not present in the JSON are decoded to
-- `Nothing`
testNothingFromMissing :: Effect Unit
testNothingFromMissing =
  let
    json = unsafeToForeign
            { tag: "UndefinedTest"
            , contents: 0
            }
  in
    case runExcept (decode json) of
      Right (UndefinedTest x) ->
        when (not (isNothing x.a))
          (throw ("Expected Nothing, got: " <> show x.a))
      Left err ->
        throw (show err)

main :: Effect Unit
main = do
  testRoundTrip (RecordTest { foo: 1, bar: "test", baz: 'a' })
  testRoundTrip (Cons 1 (Cons 2 (Cons 3 Nil)))
  testRoundTrip (UndefinedTest {a: Just "test"})
  testRoundTrip (UndefinedTest {a: Nothing})
  testRoundTrip [Just "test"]
  testRoundTrip [Nothing :: Maybe String]
  testRoundTrip (Apple)
  testRoundTrip (makeTree 0)
  testRoundTrip (makeTree 5)
  testRoundTrip (Object.fromFoldable [Tuple "one" 1, Tuple "two" 2])
  testUnaryConstructorLiteral
  let opts = defaultOptions { fieldTransform = toUpper }
  testGenericRoundTrip opts (RecordTest { foo: 1, bar: "test", baz: 'a' })
  testNothingToNull
  testNothingFromMissing
