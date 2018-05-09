module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecodeJSON, genericEncode, genericEncodeJSON)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode, encodeFields)
import Data.Foreign.Generic.EnumEncoding (class GenericDecodeEnum, class GenericEncodeEnum, GenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Data.Foreign.Generic.Types (Options, SumEncoding(..))
import Data.Foreign.JSON (parseJSON)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.String (toLower, toUpper)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Test.Assert (assert, assert', ASSERT)
import Test.Types (Fruit(..), IntList(..), Mixed(..), RecordTest(..), Tree(..), TupleArray(..), UndefinedTest(..))

buildTree :: forall a. (a -> TupleArray a a) -> Int -> a -> Tree a
buildTree _ 0 a = Leaf a
buildTree f n a = Branch $ buildTree (bimap f f) (n - 1) (f a)

-- A balanced binary tree of depth N
makeTree :: Int -> Tree Int
makeTree n = buildTree (\i -> TupleArray (Tuple (2 * i) (2 * i + 1))) n 0

throw :: forall eff. String -> Eff (assert :: ASSERT | eff) Unit
throw = flip assert' false

testRoundTrip
  :: ∀ a eff
   . Eq a
  => Decode a
  => Encode a
  => a
  -> Eff ( console :: CONSOLE
         , assert :: ASSERT
         | eff
         ) Unit
testRoundTrip x = do
  let json = encodeJSON x
  log json
  case runExcept (decodeJSON json) of
    Right y -> assert (x == y)
    Left err -> throw (show err)

testGenericRoundTrip
  :: ∀ a r eff
   . Eq a
  => Generic a r
  => GenericDecode r
  => GenericEncode r
  => Options
  -> a
  -> Eff ( console :: CONSOLE
         , assert :: ASSERT
         | eff
         ) Unit
testGenericRoundTrip opts x = do
  let json = genericEncodeJSON opts x
  log json
  case runExcept (genericDecodeJSON opts json) of
    Right y -> assert (x == y)
    Left err -> throw (show err)

testOption
  :: ∀ a rep eff
   . Eq a
  => Generic a rep
  => GenericEncodeEnum rep
  => GenericDecodeEnum rep
  => GenericEnumOptions
  -> String
  -> a
  -> Eff ( console :: CONSOLE
         , assert :: ASSERT
         | eff
         ) Unit
testOption options string value = do
  let json = unsafeStringify $ genericEncodeEnum options value
  log json
  case runExcept $ Tuple <$> decode' json <*> decode' string of
    Right (Tuple x y) -> assert (value == y && value == x)
    Left err -> throw (show err)
  where
    decode' = genericDecodeEnum options <=< parseJSON

testUnaryConstructorLiteral :: forall e.
  Eff
    ( console :: CONSOLE
    , assert :: ASSERT
    | e
    )
    Unit
testUnaryConstructorLiteral = do
    testOption (makeCasingOptions toUpper) "\"FRIKANDEL\"" Frikandel
    testOption (makeCasingOptions toLower) "\"frikandel\"" Frikandel
  where
    makeCasingOptions f =
      { constructorTagTransform: f
      }

testUnwrapSingleRecordArguments :: forall e.
  Eff
    ( console :: CONSOLE
    , assert :: ASSERT
    | e
    )
    Unit
testUnwrapSingleRecordArguments = do
  let opts = defaultOptions { unwrapSingleRecordArguments = true }

  let
    encoded = encodeJSON (genericEncode opts (Record { field1: "foo" }))
    expected = """{"field1":"foo","tag":"Record"}"""
  assert' ("Expected " <> expected <> ", got " <> encoded)
    (encoded == expected)

  testGenericRoundTrip opts (Record { field1: "foo" })
  testGenericRoundTrip opts (Other 1)

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
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
  testRoundTrip (StrMap.fromFoldable [Tuple "one" 1, Tuple "two" 2])
  testUnaryConstructorLiteral
  let opts = defaultOptions { fieldTransform = toUpper }
  testGenericRoundTrip opts (RecordTest { foo: 1, bar: "test", baz: 'a' })
  testUnwrapSingleRecordArguments
