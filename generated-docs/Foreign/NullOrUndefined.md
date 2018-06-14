## Module Foreign.NullOrUndefined

#### `readNullOrUndefined`

``` purescript
readNullOrUndefined :: forall a. (Foreign -> F a) -> Foreign -> F (Maybe a)
```

Read a value which may be null or undefined.

#### `undefined`

``` purescript
undefined :: Foreign
```


