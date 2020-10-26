module TestUtils where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, class GenericDecode, class GenericEncode, Options, decodeJSON, encodeJSON, genericDecodeJSON, genericEncodeJSON)
import Foreign.Generic.EnumEncoding (class GenericDecodeEnum, class GenericEncodeEnum, GenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Foreign.JSON (parseJSON)
import Global.Unsafe (unsafeStringify)
import Test.Assert (assert')
import Test.Unit (TestSuite, test)
import Test.Unit.Assert (equal)

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
