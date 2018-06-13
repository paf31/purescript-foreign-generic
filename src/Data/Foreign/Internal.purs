module Data.Foreign.Internal where

import Prelude

import Foreign (F, Foreign, ForeignError(..), fail, tagOf, unsafeFromForeign)
import Data.Map (Map)

type StrMap a = Map String a

-- | Test whether a foreign value is a dictionary
isStrMap :: Foreign -> Boolean
isStrMap v = tagOf v == "Object"

-- | Attempt to coerce a foreign value to a StrMap
readStrMap :: Foreign -> F (StrMap Foreign)
readStrMap value
  | isStrMap value = pure $ unsafeFromForeign value
  | otherwise = fail $ TypeMismatch "StrMap" (tagOf value)
