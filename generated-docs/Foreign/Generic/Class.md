## Module Foreign.Generic.Class

#### `GenericDecode`

``` purescript
class GenericDecode a  where
  decodeOpts :: Options -> Foreign -> F a
```

##### Instances
``` purescript
GenericDecode NoConstructors
(IsSymbol name, GenericDecodeArgs rep, GenericCountArgs rep) => GenericDecode (Constructor name rep)
(GenericDecode a, GenericDecode b) => GenericDecode (Sum a b)
```

#### `GenericEncode`

``` purescript
class GenericEncode a  where
  encodeOpts :: Options -> a -> Foreign
```

##### Instances
``` purescript
GenericEncode NoConstructors
(IsSymbol name, GenericEncodeArgs rep) => GenericEncode (Constructor name rep)
(GenericEncode a, GenericEncode b) => GenericEncode (Sum a b)
```

#### `GenericDecodeArgs`

``` purescript
class GenericDecodeArgs a  where
  decodeArgs :: Options -> Int -> List Foreign -> F { result :: a, rest :: List Foreign, next :: Int }
```

##### Instances
``` purescript
GenericDecodeArgs NoArguments
(DecodeWithOptions a) => GenericDecodeArgs (Argument a)
(GenericDecodeArgs a, GenericDecodeArgs b) => GenericDecodeArgs (Product a b)
```

#### `GenericEncodeArgs`

``` purescript
class GenericEncodeArgs a  where
  encodeArgs :: Options -> a -> List Foreign
```

##### Instances
``` purescript
GenericEncodeArgs NoArguments
(EncodeWithOptions a) => GenericEncodeArgs (Argument a)
(GenericEncodeArgs a, GenericEncodeArgs b) => GenericEncodeArgs (Product a b)
```

#### `GenericCountArgs`

``` purescript
class GenericCountArgs a  where
  countArgs :: Proxy a -> Either a Int
```

##### Instances
``` purescript
GenericCountArgs NoArguments
GenericCountArgs (Argument a)
(GenericCountArgs a, GenericCountArgs b) => GenericCountArgs (Product a b)
```


