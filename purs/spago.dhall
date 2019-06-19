{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "espial"
, dependencies =
    [ "aff"
    , "simple-json"
    , "affjax"
    , "argonaut"
    , "arrays"
    , "console"
    , "debug"
    , "effect"
    , "either"
    , "functions"
    , "halogen"
    , "prelude"
    , "psci-support"
    , "strings"
    , "transformers"
    , "web-html"
    , "profunctor-lenses"
    ]
, packages =
    ./packages.dhall
}
