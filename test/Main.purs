module Test.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecodeJSON, genericEncodeJSON)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Data.Foreign.Generic.EnumEncoding (class GenericDecodeEnum, class GenericEncodeEnum, GenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Data.Foreign.Generic.Types (Options)
import Data.Foreign.JSON (parseJSON)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (toLower, toUpper)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
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
  log $ "encoded: " <> json
  case runExcept (decodeJSON json) of
    Right y -> do
      log "OK"
      assert (x == y)
    Left err -> throw (show err)
  log ""

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
  log $ "encoded: " <> json
  case runExcept (genericDecodeJSON opts json) of
    Right y -> do
      assert (x == y)
      log "OK"
    Left err -> 
      throw (show err)
  log ""

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
  log $ "encoded: " <> json
  case runExcept $ Tuple <$> decode' json <*> decode' string of
    Right (Tuple x y) -> do
      assert (value == y && value == x)
      log "OK"
    Left err -> 
      throw (show err)
  log ""
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

main :: Effect Unit
main = do
  log "testing Apple.."
  testRoundTrip (Apple)

  log "testing Just.."
  testRoundTrip [Just "test"]

  log "testing Nothing.."
  testRoundTrip [Nothing :: Maybe String]

  log "testing constructor literal .."
  testUnaryConstructorLiteral

  log "testing RecordTest.."
  testRoundTrip (RecordTest { foo: 1, bar: "test", baz: 'a' })

  log "testing Undefined Just.."
  testRoundTrip (UndefinedTest {a: Just "test"})

  log "testing Undefined Nothing.."
  testRoundTrip (UndefinedTest {a: Nothing})

  log "testing Cons.."
  testRoundTrip (Cons 1 (Cons 2 (Cons 3 Nil)))

  log "testing empty Tree .."
  testRoundTrip (makeTree 0)

  log "testing tree 5 items .."
  testRoundTrip (makeTree 5)

  log "testing Record with fieldTransform .."
  let opts = defaultOptions { fieldTransform = toUpper }
  testGenericRoundTrip opts (RecordTest { foo: 1, bar: "test", baz: 'a' })