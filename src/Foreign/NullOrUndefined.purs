module Foreign.NullOrUndefined where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, isNull, isUndefined)

-- | Read a value which may be null or undefined.
readNullOrUndefined :: forall a. (Foreign -> F a) -> Foreign -> F (Maybe a)
readNullOrUndefined _ value | isNull value || isUndefined value = pure Nothing
readNullOrUndefined f value = Just <$> f value

foreign import undefinedImpl :: Foreign
undefined :: Foreign
undefined = undefinedImpl

foreign import nullImpl :: Foreign
null :: Foreign
null = nullImpl