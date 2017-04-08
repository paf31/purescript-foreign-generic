## Module Data.Foreign.NullOrUndefined

#### `NullOrUndefined`

``` purescript
newtype NullOrUndefined a
  = NullOrUndefined (Maybe a)
```

A `newtype` wrapper whose `IsForeign` instance correctly handles
null and undefined values.

Conceptually, this type represents values which may be `null`
or `undefined`.

##### Instances
``` purescript
Newtype (NullOrUndefined a) _
(Eq a) => Eq (NullOrUndefined a)
(Ord a) => Ord (NullOrUndefined a)
(Show a) => Show (NullOrUndefined a)
```

#### `unNullOrUndefined`

``` purescript
unNullOrUndefined :: forall a. NullOrUndefined a -> Maybe a
```

Unwrap a `NullOrUndefined` value

#### `readNullOrUndefined`

``` purescript
readNullOrUndefined :: forall a. (Foreign -> F a) -> Foreign -> F (NullOrUndefined a)
```

Read a `NullOrUndefined` value


