module BigIntegerTests where

import Prelude
import Control.Monad.Except (runExcept)
import Data.BigInteger (fromInt, readBigInteger, format)
import Data.BigInteger as BigInteger
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Ratio (reduce)
import Data.Tuple (Tuple(..))
import Foreign (unsafeToForeign)
import Foreign.Generic (ForeignError(..))
import Test.QuickCheck ((===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

all :: TestSuite
all =
  suite "BigInteger" do
    test "show(some_int) is always parsable." do
      quickCheck \x ->
        Just (fromInt x) === (BigInteger.fromString (show x))
    test "show behaves as it would for Int" do
      quickCheck \x ->
        show x === show (fromInt x)
    test "abs behaves as it would for Int" do
      quickCheck \x ->
        fromInt (abs x) === abs (fromInt x)
    test "compare behaves as it would for Int" do
      quickCheck \(Tuple x y) ->
        compare x y === compare (fromInt x) (fromInt y)
    test "Rationals" do
      equal
        (reduce (fromInt 2) (fromInt 3))
        (reduce (fromInt 50) (fromInt 75))
      equal
        (reduce (fromInt 181) (fromInt 97))
        (reduce (fromInt 362) (fromInt 194))
    suite "readBigInteger" do
      suite "should succeed" do
        test "int" do
          equal (Right zero)
            (runExcept (readBigInteger (unsafeToForeign 0)))
          equal (Right (fromInt 1234))
            (runExcept (readBigInteger (unsafeToForeign 1234)))
        test "big integer" do
          equal (Right (fromInt 123))
            (runExcept (readBigInteger (unsafeToForeign (fromInt 123))))
        test "good string" do
          equal (Right (fromInt 123))
            (runExcept (readBigInteger (unsafeToForeign "123")))
      suite "should fail" do
        test "float" do
          equal (Left (pure (TypeMismatch "bigint" "Number")))
            (runExcept (readBigInteger (unsafeToForeign 1234.6789)))
        test "object" do
          equal (Left (pure (TypeMismatch "bigint" "Object")))
            (runExcept (readBigInteger (unsafeToForeign { a: 1 })))
        test "bad string" do
          equal (Left (pure (TypeMismatch "bigint" "String")))
            (runExcept (readBigInteger (unsafeToForeign "asdf")))
    test "formatting" do
      equal "0" (format zero)
      equal "1,234,567,890" (format (fromInt 1234567890))
