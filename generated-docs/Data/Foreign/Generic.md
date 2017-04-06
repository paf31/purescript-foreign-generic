## Module Data.Foreign.Generic

#### `parseJSON`

``` purescript
parseJSON :: String -> F Foreign
```

Parse a JSON string as `Foreign` data

#### `defaultOptions`

``` purescript
defaultOptions :: Options
```

Default decoding/encoding options:

- Represent sum types as records with `tag` and `contents` fields
- Unwrap single arguments
- Don't unwrap single constructors

#### `readGeneric`

``` purescript
readGeneric :: forall a rep. Generic a rep => GenericDecode rep => Options -> Foreign -> F a
```

Read a value which has a `Generic` type.

#### `toForeignGeneric`

``` purescript
toForeignGeneric :: forall a rep. Generic a rep => GenericEncode rep => Options -> a -> Foreign
```

Generate a `Foreign` value compatible with the `readGeneric` function.

#### `readJSONGeneric`

``` purescript
readJSONGeneric :: forall a rep. Generic a rep => GenericDecode rep => Options -> String -> F a
```

Read a value which has a `Generic` type from a JSON String

#### `toJSONGeneric`

``` purescript
toJSONGeneric :: forall a rep. Generic a rep => GenericEncode rep => Options -> a -> String
```

Write a value which has a `Generic` type as a JSON String


