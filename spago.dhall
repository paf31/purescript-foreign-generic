{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "foreign-generic"
, dependencies =
    [ "assert"
    , "bigints"
    , "console"
    , "effect"
    , "exceptions"
    , "foreign"
    , "foreign-object"
    , "generics-rep"
    , "identity"
    , "test-unit"
    , "ordered-collections"
    , "proxy"
    , "psci-support"
    , "record"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
