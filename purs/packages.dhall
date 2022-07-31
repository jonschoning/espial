{-
### Overriding/Patching a package
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"
  with halogen-vdom.version = "v4.0.0"

### Additions
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-}
let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.4-20220725/src/packages.dhall
        sha256:e56fbdf33a5afd2a610c81f8b940b413a638931edb41532164e641bb2a9ec29c

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
