module Foreign.Internal.Stringify (unsafeStringify) where

-- | Uses the global JSON object to turn anything into a string. Careful! Trying
-- | to serialize functions returns undefined
foreign import unsafeStringify :: forall a. a -> String
