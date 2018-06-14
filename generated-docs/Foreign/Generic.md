## Module Foreign.Generic

#### `defaultOptions`

``` purescript
defaultOptions :: Options
```

Default decoding/encoding options:

- Represent sum types as records with `tag` and `contents` fields
- Unwrap single arguments
- Don't unwrap single constructors
- Use the constructor names as-is
- Use the field names as-is

#### `genericDecode`

``` purescript
genericDecode :: forall a rep. Generic a rep => GenericDecode rep => Options -> Foreign -> F a
```

Read a value which has a `Generic` type.

#### `genericEncode`

``` purescript
genericEncode :: forall a rep. Generic a rep => GenericEncode rep => Options -> a -> Foreign
```

Generate a `Foreign` value compatible with the `genericDecode` function.

#### `decodeJSON`

``` purescript
decodeJSON :: forall a. Decode a => String -> F a
```

Decode a JSON string using a `Decode` instance.

#### `encodeJSON`

``` purescript
encodeJSON :: forall a. Encode a => a -> String
```

Encode a JSON string using an `Encode` instance.

#### `genericDecodeJSON`

``` purescript
genericDecodeJSON :: forall a rep. Generic a rep => GenericDecode rep => Options -> String -> F a
```

Read a value which has a `Generic` type from a JSON String

#### `genericEncodeJSON`

``` purescript
genericEncodeJSON :: forall a rep. Generic a rep => GenericEncode rep => Options -> a -> String
```

Write a value which has a `Generic` type as a JSON String


