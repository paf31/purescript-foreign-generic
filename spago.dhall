{ name = "foreign-generic"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "prelude"
  , "tuples"
  , "bifunctors"
  , "foreign"
  , "foreign-object"
  , "assert"
  , "record"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
