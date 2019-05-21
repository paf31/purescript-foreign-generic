-- | This module is provided for backwards-compatibility with the old API.
-- |
-- | It is liable to be removed in a future release. 

module Foreign.Class
  ( module Reexports
  ) where

import Foreign.Generic.Class (class Decode, class Encode, decode, encode) as Reexports
