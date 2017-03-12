# purescript-foreign-generic

[![Build Status](https://travis-ci.org/paf31/purescript-foreign-generic.svg?branch=master)](https://travis-ci.org/paf31/purescript-foreign-generic)

Generic deriving for `purescript-foreign`.

- [Module Documentation](docs/Data/Foreign/Generic.md)
- [Example](test/Main.purs)
- [Further examples in this repo](https://github.com/justinwoo/purescript-howto-foreign-generic)

## Example Usage

```purescript
import Data.Foreign.Class (class AsForeign, class IsForeign, readJSON, write)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype MyRecord = MyRecord {a :: Int}

derive instance genericMyRecord :: Generic MyRecord _

instance isForeignMyRecord :: IsForeign MyRecord where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}

instance asForeignMyRecord :: AsForeign MyRecord where
  write = toForeignGeneric $ defaultOptions {unwrapSingleConstructors = true}

toJSONString = write >>> unsafeStringify
fromJSONString = readJSON >>> runExcept

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ toJSONString (MyRecord {a: 1})
  -- {a: 1}

  log $ show eMyRecord
  -- Right (MyRecord {a: 1})
  where
    eMyRecord :: Either _ MyRecord
    eMyRecord = fromJSONString """{"a": 1}"""
```