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
(Decode_ a) => GenericDecodeArgs (Argument a)
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
(Encode_ a) => GenericEncodeArgs (Argument a)
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

#### `Decode_`

``` purescript
class Decode_ a  where
  decode_ :: Options -> Foreign -> F a
```

##### Instances
``` purescript
(RowToList r rl, DecodeRecord r rl) => Decode_ {  | r }
(Decode a) => Decode_ a
```

#### `Encode_`

``` purescript
class Encode_ a  where
  encode_ :: Options -> a -> Foreign
```

##### Instances
``` purescript
(RowToList r rl, EncodeRecord r rl) => Encode_ {  | r }
(Encode a) => Encode_ a
```

#### `DecodeRecord`

``` purescript
class DecodeRecord r rl | rl -> r where
  decodeRecord_ :: RLProxy rl -> Options -> Foreign -> F (Builder {  } ({  | r }))
```

##### Instances
``` purescript
DecodeRecord () Nil
(Cons l a r_ r, DecodeRecord r_ rl_, IsSymbol l, Decode_ a, Lacks l r_) => DecodeRecord r (Cons l a rl_)
```

#### `EncodeRecord`

``` purescript
class EncodeRecord r rl | rl -> r where
  encodeRecord_ :: RLProxy rl -> Options -> {  | r } -> Object Foreign
```

##### Instances
``` purescript
EncodeRecord () Nil
(Cons l a r_ r, EncodeRecord r_ rl_, IsSymbol l, Encode_ a) => EncodeRecord r (Cons l a rl_)
```


