module Data.BigInteger
  ( BigInteger
  , fromString
  , fromInt
  , toNumber
  , format
  , readBigInteger
  ) where

import Control.Applicative (pure)
import Data.CommutativeRing (class CommutativeRing)
import Data.Eq (class Eq)
import Data.EuclideanRing (class EuclideanRing)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord, compare)
import Data.Ring (class Ring)
import Data.Semiring (class Semiring)
import Data.Show (class Show)
import Foreign (fail, tagOf, unsafeToForeign)
import Foreign.Generic (F, Foreign, ForeignError(..))
import Foreign.Generic.Class (class Decode, class Encode)

foreign import data BigInteger :: Type

------------------------------------------------------------
foreign import eq_ :: BigInteger -> BigInteger -> Boolean

instance eqBigInteger :: Eq BigInteger where
  eq = eq_

foreign import comparedTo_ :: BigInteger -> BigInteger -> Int

instance ordBigInteger :: Ord BigInteger where
  compare x y = compare (comparedTo_ x y) 0

foreign import show_ :: BigInteger -> String

instance showBigInteger :: Show BigInteger where
  show = show_

------------------------------------------------------------
foreign import add_ :: BigInteger -> BigInteger -> BigInteger

foreign import mul_ :: BigInteger -> BigInteger -> BigInteger

instance semiringBigInteger :: Semiring BigInteger where
  zero = fromInt 0
  one = fromInt 1
  add = add_
  mul = mul_

foreign import sub_ :: BigInteger -> BigInteger -> BigInteger

instance ringBigInteger :: Ring BigInteger where
  sub = sub_

foreign import div_ :: BigInteger -> BigInteger -> BigInteger

foreign import mod_ :: BigInteger -> BigInteger -> BigInteger

foreign import degree_ :: BigInteger -> Int

instance commutativeRingBigInteger :: CommutativeRing BigInteger

instance euclideanRingBigInteger :: EuclideanRing BigInteger where
  div = div_
  mod = mod_
  degree = degree_

------------------------------------------------------------
foreign import fromInt :: Int -> BigInteger

foreign import toNumber :: BigInteger -> Number

foreign import fromString_ :: Maybe BigInteger -> (BigInteger -> Maybe BigInteger) -> String -> Maybe BigInteger

fromString :: String -> Maybe BigInteger
fromString = fromString_ Nothing Just

------------------------------------------------------------
instance bigIntegerDecode :: Decode BigInteger where
  decode = readBigInteger

instance bigIntegerEncode :: Encode BigInteger where
  encode = unsafeToForeign

foreign import readBigInteger_ :: Maybe BigInteger -> (BigInteger -> Maybe BigInteger) -> Foreign -> Maybe BigInteger

-- | Attempt to coerce a foreign value to a `BigInteger`.
readBigInteger :: Foreign -> F BigInteger
readBigInteger value = case readBigInteger_ Nothing Just value of
  Just n -> pure n
  Nothing -> fail $ TypeMismatch "bigint" (tagOf value)

------------------------------------------------------------
foreign import format :: BigInteger -> String
