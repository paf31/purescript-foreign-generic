module Data.Foreign.NullOrUndefined where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Maybe (Maybe(..))
import Data.Foreign (F, Foreign, isUndefined, isNull)

-- | A `newtype` wrapper whose `IsForeign` instance correctly handles
-- | null and undefined values.
-- |
-- | Conceptually, this type represents values which may be `null`
-- | or `undefined`.
newtype NullOrUndefined a = NullOrUndefined (Maybe a)

derive instance newtypeNullOrUndefined :: Newtype (NullOrUndefined a) _
derive instance eqNullOrUndefined :: Eq a => Eq (NullOrUndefined a)
derive instance ordNullOrUndefined :: Ord a => Ord (NullOrUndefined a)
derive newtype instance functorNullOrUndefined :: Functor NullOrUndefined
derive newtype instance applyNullOrUndefined :: Apply NullOrUndefined
derive newtype instance applicativeNullOrUndefined :: Applicative NullOrUndefined
derive newtype instance bindNullOrUndefined :: Bind NullOrUndefined
instance monadNullOrUndefined :: Monad NullOrUndefined

instance showNullOrUndefined :: (Show a) => Show (NullOrUndefined a) where
  show x = "(NullOrUndefined " <> show (unwrap x) <> ")"

-- | Read a `NullOrUndefined` value
readNullOrUndefined :: forall a. (Foreign -> F a) -> Foreign -> F (NullOrUndefined a)
readNullOrUndefined _ value | isNull value || isUndefined value = pure (NullOrUndefined Nothing)
readNullOrUndefined f value = NullOrUndefined <<< Just <$> f value

foreign import undefined :: Foreign
foreign import null :: Foreign
