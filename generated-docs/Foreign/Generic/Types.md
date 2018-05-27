## Module Foreign.Generic.Types

#### `Options`

``` purescript
type Options = { sumEncoding :: SumEncoding, unwrapSingleConstructors :: Boolean, unwrapSingleArguments :: Boolean, fieldTransform :: String -> String }
```

#### `SumEncoding`

``` purescript
data SumEncoding
  = TaggedObject { tagFieldName :: String, contentsFieldName :: String, constructorTagTransform :: String -> String }
```

The encoding of sum types for your type.
`TaggedObject`s will be encoded in the form `{ [tagFieldName]: "ConstructorTag", [contentsFieldName]: "Contents"}`.
`constructorTagTransform` can be provided to transform the constructor tag to a form you use, e.g. `toLower`/`toUpper`.


