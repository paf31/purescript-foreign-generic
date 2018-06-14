module Foreign.Internal where

import Prelude

import Foreign (F, Foreign, ForeignError(..), fail, tagOf, unsafeFromForeign)
import Foreign.Object (Object)

-- | Test whether a foreign value is a dictionary
isObject :: Foreign -> Boolean
isObject v = tagOf v == "Object"

-- | Attempt to coerce a foreign value to an `Object`.
readObject :: Foreign -> F (Object Foreign)
readObject value
  | isObject value = pure $ unsafeFromForeign value
  | otherwise = fail $ TypeMismatch "Object" (tagOf value)
