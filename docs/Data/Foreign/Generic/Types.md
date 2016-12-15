## Module Data.Foreign.Generic.Types

#### `Options`

``` purescript
type Options = { sumEncoding :: SumEncoding, unwrapSingleConstructors :: Boolean, unwrapSingleArguments :: Boolean }
```

#### `SumEncoding`

``` purescript
data SumEncoding
  = TaggedObject { tagFieldName :: String, contentsFieldName :: String }
```


