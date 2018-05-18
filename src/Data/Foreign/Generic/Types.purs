module Data.Foreign.Generic.Types where

-- | Encoding options.
-- | 
-- | ### `unwrapSingleConstructors`
-- | 
-- | If true, data types with only one constructor will be encoded
-- | without the constructor tag.
-- | 
-- | Example:
-- | 
-- | ```purescript
-- | data SC = SC Int Int
-- | -- encoding (SC 1 2)
-- | ```
-- | 
-- | Without `unwrapSingleConstructors`:
-- | 
-- | `{ "tag": "SC, "contents": [1, 2] }`
-- | 
-- | With `unwrapSingleConstructors`:
-- | 
-- | `[1, 2]`
-- | 
-- | ### `unwrapSingleArguments`
-- | 
-- | In the general case, constructor arguments are encoded as an array.
-- | 
-- | If this option is enabled, for constructors with only one argument the argument
-- | will not be wrapped in a single-element array.
-- | 
-- | Example:
-- | 
-- | ```purescript
-- | data D = C1 Int | C2 Int
-- | -- encoding (C2 1)
-- | ```
-- | 
-- | Without `unwrapSingleArguments`:
-- | 
-- | `{ "tag": "C2", "contents": [1] }`
-- | 
-- | With `unwrapSingleArguments`:
-- | 
-- | `{ "tag": "C2", "contents": 1 }`
-- | 
-- | ### `unwrapSingleRecordArguments`
-- | 
-- | If this option is enabled, constructors where the only argument is a record
-- | will be encoded without wrapping the record in a `contents` property;
-- | instead, the record fields will be directly attached to the main object.
-- |
-- | This is the encoding used by `aeson` `genericToJSON`.
-- | 
-- | Example:
-- | 
-- | ```purescript
-- | data D = C1 { x :: Int, y :: Int } | C2 { x :: Int }
-- | -- encoding (C1 { x: 1, y: 2 })
-- | ```
-- | 
-- | Without `unwrapSingleRecordArguments`:
-- | 
-- | `{ "tag": "C1", "contents": { "x": 1, "y": 2 } }`
-- | 
-- | With `unwrapSingleRecordArguments`:
-- | 
-- | `{ "tag": "C1", "x": 1, "y": 2 }`
-- | 
-- | ### `fieldTransform`
-- | 
-- | A transformation function on record field names.
type Options =
  { sumEncoding :: SumEncoding
  , unwrapSingleConstructors :: Boolean
  , unwrapSingleArguments :: Boolean
  , unwrapSingleRecordArguments :: Boolean
  , fieldTransform :: String -> String
  }

-- | The encoding of sum types for your type.
-- | `TaggedObject`s will be encoded in the form `{ [tagFieldName]: "ConstructorTag", [contentsFieldName]: "Contents"}`.
-- | `constructorTagTransform` can be provided to transform the constructor tag to a form you use, e.g. `toLower`/`toUpper`.
data SumEncoding
  = TaggedObject
    { tagFieldName :: String
    , contentsFieldName :: String
    , constructorTagTransform :: String -> String
    }
