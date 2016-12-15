module Data.Foreign.Generic.Types where

type Options =
  { sumEncoding :: SumEncoding
  , unwrapSingleConstructors :: Boolean
  , unwrapSingleArguments :: Boolean
  }

data SumEncoding
  = TaggedObject
    { tagFieldName :: String
    , contentsFieldName :: String
    }
