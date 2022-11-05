let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.4-20221026/src/packages.dhall
        sha256:8dc0b394f5861bb0136f652f3f826a88eaffb2bc0ecf0251468ed668102f5d0c

-- ### Additions
let upstream = upstream
in  upstream
  with simple-json =
    { dependencies =
      [ "arrays"
      , "exceptions"
      , "foreign"
      , "foreign-object"
      , "nullable"
      , "prelude"
      , "record"
      , "typelevel-prelude"
      , "variant"
      ]
    , repo = "https://github.com/justinwoo/purescript-simple-json.git"
    , version = "v9.0.0"
    }
  with autocomplete =
    { dependencies =
      [ "aff"
      , "arrays"
      , "effect"
      , "either"
      , "foreign-object"
      , "lists"
      , "nonempty"
      , "prelude"
      , "signal"
      , "strings"
      , "tuples"
      ]
    , repo =
       "https://github.com/spicydonuts/purescript-autocomplete"
    , version =
        "80dd84c45c5aefb79e4a1cf909cbaadf3d816230"
    }
