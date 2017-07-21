module Test.Main where

import Prelude
import Data.Map as Map
import Data.StrMap as StrMap
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(..), fail)
import Data.Foreign.Class (class Decode, class DecodeKey, class Encode, class EncodeKey)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Foreign.Generic.EnumEncoding (class GenericDecodeEnum, class GenericEncodeEnum, GenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Maybe (Maybe(..))
import Data.String (toLower, toUpper)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Test.Assert (assert, assert', ASSERT)
import Test.Types (Fruit(..), IntList(..), RecordTest(..), Tree(..), TupleArray(..), UndefinedTest(..))

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


data Flavor
  = Strawberry
  | Melon

derive instance flavorGeneric :: Generic Flavor _

instance flavorEq :: Eq Flavor where eq = genericEq

instance flavorOrd :: Ord Flavor where compare = genericCompare

instance flavorDecodeKey :: DecodeKey Flavor where
  decodeKey "strawberry" = pure Strawberry
  decodeKey "melon" = pure Melon
  decodeKey s = fail $ ForeignError $ "no such flavor:" <> s

instance flavorEncodeKey :: EncodeKey Flavor where
  encodeKey Strawberry = "strawberry"
  encodeKey Melon = "melon"

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  testRoundTrip (RecordTest { foo: 1, bar: "test", baz: 'a' })
  testRoundTrip (Cons 1 (Cons 2 (Cons 3 Nil)))
  testRoundTrip (UndefinedTest {a: NullOrUndefined (Just "test")})
  testRoundTrip (UndefinedTest {a: NullOrUndefined Nothing})
  testRoundTrip [NullOrUndefined (Just "test")]
  testRoundTrip [NullOrUndefined (Nothing :: Maybe String)]
  testRoundTrip (Apple)
  testRoundTrip (makeTree 0)
  testRoundTrip (makeTree 5)
  testRoundTrip (StrMap.fromFoldable [Tuple "one" 1, Tuple "two" 2])
  testRoundTrip (Map.fromFoldable [Tuple Strawberry 1, Tuple Melon 2])
  testRoundTrip (Map.fromFoldable [Tuple "one" "uno", Tuple "two" "dos"])
  testRoundTrip (Map.fromFoldable [Tuple 1 2, Tuple 2 4])
  testRoundTrip (Map.fromFoldable [Tuple true 2, Tuple false 4])
  testRoundTrip (Map.fromFoldable [Tuple 'a' 2, Tuple 'b' 4])        
  testUnaryConstructorLiteral
