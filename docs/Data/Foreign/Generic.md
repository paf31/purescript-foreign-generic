## Module Data.Foreign.Generic

#### `readGeneric`

``` purescript
readGeneric :: forall a. (Generic a) => Foreign -> F a
```

Read a value which has a `Generic` type.

#### `readJSONGeneric`

``` purescript
readJSONGeneric :: forall a. (Generic a) => String -> F a
```

Read a value which has a `Generic` type from a JSON String


