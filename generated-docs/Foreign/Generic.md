## Module Foreign.Generic

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


### Re-exported from Foreign:

#### `ForeignError`

``` purescript
data ForeignError
  = ForeignError String
  | TypeMismatch String String
  | ErrorAtIndex Int ForeignError
  | ErrorAtProperty String ForeignError
```

A type for foreign type errors

##### Instances
``` purescript
Eq ForeignError
Ord ForeignError
Show ForeignError
```

#### `Foreign`

``` purescript
data Foreign :: Type
```

A type for _foreign data_.

Foreign data is data from any external _unknown_ or _unreliable_
source, for which it cannot be guaranteed that the runtime representation
conforms to that of any particular type.

Suitable applications of `Foreign` are

- To represent responses from web services
- To integrate with external JavaScript libraries.

#### `F`

``` purescript
type F = Except MultipleErrors
```

An error monad, used in this library to encode possible failures when
dealing with foreign data.

The `Alt` instance for `Except` allows us to accumulate errors,
unlike `Either`, which preserves only the last error.

### Re-exported from Foreign.Generic.Class:

#### `SumEncoding`

``` purescript
data SumEncoding
  = TaggedObject { tagFieldName :: String, contentsFieldName :: String, constructorTagTransform :: String -> String }
```

The encoding of sum types for your type.
`TaggedObject`s will be encoded in the form `{ [tagFieldName]: "ConstructorTag", [contentsFieldName]: "Contents"}`.
`constructorTagTransform` can be provided to transform the constructor tag to a form you use, e.g. `toLower`/`toUpper`.

#### `Options`

``` purescript
type Options = { sumEncoding :: SumEncoding, unwrapSingleConstructors :: Boolean, unwrapSingleArguments :: Boolean, fieldTransform :: String -> String }
```

Encoding/Decoding options which can be used to customize
`Decode` and `Encode` instances which are derived via
`Generic` (see `genericEncode` and `genericDecode`).

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

#### `GenericDecode`

``` purescript
class GenericDecode a 
```

##### Instances
``` purescript
GenericDecode NoConstructors
(IsSymbol name, GenericDecodeArgs rep, GenericCountArgs rep) => GenericDecode (Constructor name rep)
(GenericDecode a, GenericDecode b) => GenericDecode (Sum a b)
```

#### `GenericEncode`

``` purescript
class GenericEncode a 
```

##### Instances
``` purescript
GenericEncode NoConstructors
(IsSymbol name, GenericEncodeArgs rep) => GenericEncode (Constructor name rep)
(GenericEncode a, GenericEncode b) => GenericEncode (Sum a b)
```

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

