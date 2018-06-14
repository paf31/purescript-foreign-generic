## Module Foreign.Generic.EnumEncoding

#### `GenericEnumOptions`

``` purescript
type GenericEnumOptions = { constructorTagTransform :: String -> String }
```

#### `defaultGenericEnumOptions`

``` purescript
defaultGenericEnumOptions :: GenericEnumOptions
```

#### `genericDecodeEnum`

``` purescript
genericDecodeEnum :: forall a rep. Generic a rep => GenericDecodeEnum rep => GenericEnumOptions -> Foreign -> F a
```

A generic function to be used with "Enums", or sum types with only no-argument constructors. This is used for decoding from strings to one of the constructors, combined with the `constructorTagTransform` property of `SumEncoding`.

#### `genericEncodeEnum`

``` purescript
genericEncodeEnum :: forall a rep. Generic a rep => GenericEncodeEnum rep => GenericEnumOptions -> a -> Foreign
```

A generic function to be used with "Enums", or sum types with only no-argument constructors. This is used for encoding to strings from one of the constructors, combined with the `constructorTagTransform` property of `SumEncoding`.

For example:

```purescript
data Fruit = Apple | Banana | Frikandel
derive instance geFruit :: Generic Fruit _
instance eFruit :: Encode Fruit where
  encode = genericEncodeEnum defaultGenericEnumOptions

#### `GenericDecodeEnum`

``` purescript
class GenericDecodeEnum a  where
  decodeEnum :: GenericEnumOptions -> Foreign -> F a
```

A type class for type representations that can be used for decoding to an Enum. Only the sum and no-argument constructor instances are valid, while others provide a `Fail` constraint to fail in compilation.

For example:

```purescript
data Fruit = Apple | Banana | Frikandel
derive instance geFruit :: Generic Fruit _
instance dFruit :: Decode Fruit where
  decode = genericDecodeEnum defaultGenericEnumOptions
```

##### Instances
``` purescript
(GenericDecodeEnum a, GenericDecodeEnum b) => GenericDecodeEnum (Sum a b)
(IsSymbol name) => GenericDecodeEnum (Constructor name NoArguments)
(Fail (Text "genericEncode/DecodeEnum cannot be used on types that are not sums of constructors with no arguments.")) => GenericDecodeEnum (Constructor name (Argument a))
(Fail (Text "genericEncode/DecodeEnum cannot be used on types that are not sums of constructors with no arguments.")) => GenericDecodeEnum (Constructor name (Product a b))
```

#### `GenericEncodeEnum`

``` purescript
class GenericEncodeEnum a  where
  encodeEnum :: GenericEnumOptions -> a -> Foreign
```

A type class for type representations that can be used for encoding from an Enum. Only the sum and no-argument constructor instances are valid, while others provide a `Fail` constraint to fail in compilation.

##### Instances
``` purescript
(GenericEncodeEnum a, GenericEncodeEnum b) => GenericEncodeEnum (Sum a b)
(IsSymbol name) => GenericEncodeEnum (Constructor name NoArguments)
(Fail (Text "genericEncode/DecodeEnum cannot be used on types that are not sums of constructors with no arguments.")) => GenericEncodeEnum (Constructor name (Argument a))
(Fail (Text "genericEncode/DecodeEnum cannot be used on types that are not sums of constructors with no arguments.")) => GenericEncodeEnum (Constructor name (Product a b))
```


