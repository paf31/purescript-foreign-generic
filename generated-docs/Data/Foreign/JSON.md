## Module Data.Foreign.JSON

#### `parseJSON`

``` purescript
parseJSON :: String -> F Foreign
```

Parse a JSON string as `Foreign` data

#### `decodeJSONWith`

``` purescript
decodeJSONWith :: forall a. (Foreign -> F a) -> String -> F a
```


