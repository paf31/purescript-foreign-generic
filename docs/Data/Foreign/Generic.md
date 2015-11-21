## Module Data.Foreign.Generic

#### `Options`

``` purescript
type Options = { sumEncoding :: SumEncoding, unwrapNewtypes :: Boolean, unwrapSingleArgumentConstructors :: Boolean, maybeAsNull :: Boolean, tupleAsArray :: Boolean }
```

#### `SumEncoding`

``` purescript
data SumEncoding
  = TaggedObject { tagFieldName :: String, contentsFieldName :: String }
```

#### `defaultOptions`

``` purescript
defaultOptions :: Options
```

#### `readGeneric`

``` purescript
readGeneric :: forall a. (Generic a) => Options -> Foreign -> F a
```

Read a value which has a `Generic` type.

#### `toForeignGeneric`

``` purescript
toForeignGeneric :: forall a. (Generic a) => Options -> a -> Foreign
```

Generate a `Foreign` value compatible with the `readGeneric` function.

#### `readJSONGeneric`

``` purescript
readJSONGeneric :: forall a. (Generic a) => Options -> String -> F a
```

Read a value which has a `Generic` type from a JSON String

#### `toJSONGeneric`

``` purescript
toJSONGeneric :: forall a. (Generic a) => Options -> a -> String
```

Write a value which has a `Generic` type as a JSON String


