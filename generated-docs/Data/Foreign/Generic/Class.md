## Module Data.Foreign.Generic.Class

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
  decodeArgs :: Int -> List Foreign -> F { result :: a, rest :: List Foreign, next :: Int }
```

##### Instances
``` purescript
GenericDecodeArgs NoArguments
(Decode a) => GenericDecodeArgs (Argument a)
(GenericDecodeArgs a, GenericDecodeArgs b) => GenericDecodeArgs (Product a b)
(GenericDecodeFields fields) => GenericDecodeArgs (Rec fields)
```

#### `GenericEncodeArgs`

``` purescript
class GenericEncodeArgs a  where
  encodeArgs :: a -> List Foreign
```

##### Instances
``` purescript
GenericEncodeArgs NoArguments
(Encode a) => GenericEncodeArgs (Argument a)
(GenericEncodeArgs a, GenericEncodeArgs b) => GenericEncodeArgs (Product a b)
(GenericEncodeFields fields) => GenericEncodeArgs (Rec fields)
```

#### `GenericDecodeFields`

``` purescript
class GenericDecodeFields a  where
  decodeFields :: Foreign -> F a
```

##### Instances
``` purescript
(IsSymbol name, Decode a) => GenericDecodeFields (Field name a)
(GenericDecodeFields a, GenericDecodeFields b) => GenericDecodeFields (Product a b)
```

#### `GenericEncodeFields`

``` purescript
class GenericEncodeFields a  where
  encodeFields :: a -> StrMap Foreign
```

##### Instances
``` purescript
(IsSymbol name, Encode a) => GenericEncodeFields (Field name a)
(GenericEncodeFields a, GenericEncodeFields b) => GenericEncodeFields (Product a b)
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
GenericCountArgs (Rec fields)
```


