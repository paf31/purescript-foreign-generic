module Test.Main where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.String (toLower, toUpper)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign (isNull, unsafeToForeign)
import Foreign.Generic (class Decode, class Encode, class GenericDecode, class GenericEncode, Options, decode, encode, defaultOptions, decodeJSON, encodeJSON, genericDecodeJSON, genericEncodeJSON)
import Foreign.Generic.EnumEncoding (class GenericDecodeEnum, class GenericEncodeEnum, GenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Foreign.Index (readProp)
import Foreign.JSON (parseJSON)
import Foreign.Object as Object
import Global.Unsafe (unsafeStringify)
import Test.Assert (assert')
import Test.Types (Fruit(..), IntList(..), RecordTest(..), Tree(..), TupleArray(..), UndefinedTest(..), SumWithRecord(..))
import Test.Unit (TestSuite, failure, success, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

buildTree :: forall a. (a -> TupleArray a a) -> Int -> a -> Tree a
buildTree _ 0 a = Leaf a

buildTree f n a = Branch $ buildTree (bimap f f) (n - 1) (f a)

-- A balanced binary tree of depth N
makeTree :: Int -> Tree Int
makeTree n = buildTree (\i -> TupleArray (Tuple (2 * i) (2 * i + 1))) n 0

throw :: String -> Effect Unit
throw = flip assert' false

testRoundTrip ::
  ∀ a.
  Eq a =>
  Show a =>
  Decode a =>
  Encode a =>
  a ->
  TestSuite
testRoundTrip x =
  test ("RoundTrip " <> show x) do
    equal (Right x) (runExcept (decodeJSON (encodeJSON x)))

testGenericRoundTrip ::
  ∀ a r.
  Eq a =>
  Show a =>
  Generic a r =>
  GenericDecode r =>
  GenericEncode r =>
  Options ->
  a ->
  TestSuite
testGenericRoundTrip opts x =
  test ("Generic roundtrip " <> show x) do
    equal (Right x) (runExcept (genericDecodeJSON opts (genericEncodeJSON opts x)))

testOption ::
  ∀ a rep.
  Eq a =>
  Show a =>
  Generic a rep =>
  GenericEncodeEnum rep =>
  GenericDecodeEnum rep =>
  GenericEnumOptions ->
  String ->
  a ->
  TestSuite
testOption options string value =
  test "testOption" do
    let
      json = unsafeStringify $ genericEncodeEnum options value
    equal (Right value) (runExcept (decode' json))
    equal (Right value) (runExcept (decode' string))
  where
  decode' = genericDecodeEnum options <=< parseJSON

testUnaryConstructorLiteral :: TestSuite
testUnaryConstructorLiteral = do
  testOption (makeCasingOptions toUpper) "\"FRIKANDEL\"" Frikandel
  testOption (makeCasingOptions toLower) "\"frikandel\"" Frikandel
  where
  makeCasingOptions f =
    { constructorTagTransform: f
    }

-- Test that `Nothing` record fields, when encoded to JSON, are present and
-- encoded as `null`
testNothingToNull :: TestSuite
testNothingToNull =
  test "Nothing to Null" do
    let json = encode (UndefinedTest { a: Nothing })
    case runExcept (pure json >>= readProp "contents" >>= readProp "a") of
      Right val ->
        if (isNull val) then
          success
        else
          failure ("property 'a' was not null; got: " <> encodeJSON val)
      Left err -> failure (show err)

-- Test that `Maybe` fields which are not present in the JSON are decoded to
-- `Nothing`
testNothingFromMissing :: TestSuite
testNothingFromMissing =
  test "Nothing from missing" do
    let
      json =
        unsafeToForeign
          { tag: "UndefinedTest"
          , contents: 0
          }
    case runExcept (decode json) of
      Right (UndefinedTest x) ->
        if (isNothing x.a) then
          success
        else
          failure ("Expected Nothing, got: " <> show x.a)
      Left err -> failure (show err)

main :: Effect Unit
main =
  runTest do
    suite "RoundTrips" do
      testRoundTrip (RecordTest { foo: 1, bar: "test", baz: 'a' })
      testRoundTrip NoArgs
      testRoundTrip (SomeArg "some argument")
      testRoundTrip (ManyArgs "fst" "snd")
      testRoundTrip (RecordArgs { foo: 1, bar: "test", baz: 'a' })
      testRoundTrip (Cons 1 (Cons 2 (Cons 3 Nil)))
      testRoundTrip (UndefinedTest { a: Just "test" })
      testRoundTrip (UndefinedTest { a: Nothing })
      testRoundTrip [ Just "test" ]
      testRoundTrip [ Nothing :: Maybe String ]
      testRoundTrip (Apple)
      testRoundTrip (makeTree 0)
      testRoundTrip (makeTree 5)
      testRoundTrip (Object.fromFoldable [ Tuple "one" 1, Tuple "two" 2 ])
      testRoundTrip (Map.fromFoldable [ Tuple "one" 1, Tuple "two" 2 ])
      test "Maps" do
        equal (Right (Map.fromFoldable [ Tuple "foo" 5 ]))
          (runExcept (decodeJSON "{\"foo\": 5}"))
        equal (Right (Map.empty :: Map String Int))
          (runExcept (decodeJSON "null"))
      testRoundTrip [ Left 5, Right "Test" ]
      testRoundTrip (BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 60)) -- 2^60. Anything over 2^32 would baffle JavaScript.
      test "BigInt" do
        equal (Right (BigInt.fromInt 50))
          (runExcept (decodeJSON "50"))
      testUnaryConstructorLiteral
      let
        opts = defaultOptions { fieldTransform = toUpper }
      pure unit
      testGenericRoundTrip opts (RecordTest { foo: 1, bar: "test", baz: 'a' })
      testNothingToNull
      testNothingFromMissing
