## Module Foreign.Generic.Class

#### `Options`

``` purescript
type Options = { sumEncoding :: SumEncoding, unwrapSingleConstructors :: Boolean, unwrapSingleArguments :: Boolean, fieldTransform :: String -> String }
```

Encoding/Decoding options which can be used to customize
`Decode` and `Encode` instances which are derived via
`Generic` (see `genericEncode` and `genericDecode`).

#### `SumEncoding`

``` purescript
data SumEncoding
  = TaggedObject { tagFieldName :: String, contentsFieldName :: String, constructorTagTransform :: String -> String }
```

The encoding of sum types for your type.
`TaggedObject`s will be encoded in the form `{ [tagFieldName]: "ConstructorTag", [contentsFieldName]: "Contents"}`.
`constructorTagTransform` can be provided to transform the constructor tag to a form you use, e.g. `toLower`/`toUpper`.

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

#### `Decode`

``` purescript
class Decode a  where
  decode :: Foreign -> F a
```

The `Decode` class is used to generate decoding functions
of the form `Foreign -> F a` using `generics-rep` deriving.

First, derive `Generic` for your data:

```purescript
import Data.Generic.Rep

data MyType = MyType ...

derive instance genericMyType :: Generic MyType _
```

You can then use the `genericDecode` and `genericDecodeJSON` functions
to decode your foreign/JSON-encoded data.

##### Instances
``` purescript
Decode Void
Decode Unit
Decode Foreign
Decode String
Decode Char
Decode Boolean
Decode Number
Decode Int
(Decode a) => Decode (Identity a)
(Decode a) => Decode (Array a)
(Decode a) => Decode (Maybe a)
(Decode v) => Decode (Object v)
(RowToList r rl, DecodeRecord r rl) => Decode {  | r }
```

#### `Encode`

``` purescript
class Encode a  where
  encode :: a -> Foreign
```

The `Encode` class is used to generate encoding functions
of the form `a -> Foreign` using `generics-rep` deriving.

First, derive `Generic` for your data:

```purescript
import Data.Generic.Rep

data MyType = MyType ...

derive instance genericMyType :: Generic MyType _
```

You can then use the `genericEncode` and `genericEncodeJSON` functions
to encode your data as JSON.

##### Instances
``` purescript
Encode Void
Encode Unit
Encode Foreign
Encode String
Encode Char
Encode Boolean
Encode Number
Encode Int
(Encode a) => Encode (Identity a)
(Encode a) => Encode (Array a)
(Encode a) => Encode (Maybe a)
(Encode v) => Encode (Object v)
(RowToList r rl, EncodeRecord r rl) => Encode {  | r }
```

#### `DecodeWithOptions`

``` purescript
class DecodeWithOptions a  where
  decodeWithOptions :: Options -> Foreign -> F a
```

When deriving `En`/`Decode` instances using `Generic`, we want
the `Options` object to apply to the outermost record type(s)
under the data constructors.

For this reason, we cannot use `En`/`Decode` directly when we
reach an `Argument` during generic traversal of a type, because it
might be a record type. Instead, we need to peel off any record
type(s) and apply the appropriate `Options` before we can delegate
to `En`/`Decode`, which can bake in its own `Options`.

##### Instances
``` purescript
(RowToList r rl, DecodeRecord r rl) => DecodeWithOptions {  | r }
(Decode a) => DecodeWithOptions a
```

#### `EncodeWithOptions`

``` purescript
class EncodeWithOptions a  where
  encodeWithOptions :: Options -> a -> Foreign
```

See the comment on `DecodeWithOptions`.

##### Instances
``` purescript
(RowToList r rl, EncodeRecord r rl) => EncodeWithOptions {  | r }
(Encode a) => EncodeWithOptions a
```

#### `DecodeRecord`

``` purescript
class DecodeRecord r rl | rl -> r where
  decodeRecordWithOptions :: RLProxy rl -> Options -> Foreign -> F (Builder {  } ({  | r }))
```

##### Instances
``` purescript
DecodeRecord () Nil
(Cons l a r_ r, DecodeRecord r_ rl_, IsSymbol l, DecodeWithOptions a, Lacks l r_) => DecodeRecord r (Cons l a rl_)
```

#### `EncodeRecord`

``` purescript
class EncodeRecord r rl | rl -> r where
  encodeRecordWithOptions :: RLProxy rl -> Options -> {  | r } -> Object Foreign
```

##### Instances
``` purescript
EncodeRecord () Nil
(Cons l a r_ r, EncodeRecord r_ rl_, IsSymbol l, EncodeWithOptions a) => EncodeRecord r (Cons l a rl_)
```

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


