## Module Foreign.Internal

#### `isObject`

``` purescript
isObject :: Foreign -> Boolean
```

Test whether a foreign value is a dictionary

#### `readObject`

``` purescript
readObject :: Foreign -> F (Object Foreign)
```

Attempt to coerce a foreign value to an `Object`.


