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
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.14-20240117/src/packages.dhall
        sha256:2ca7a513183cb6273394fc328b0b891044c159af7d9bc11ee7c7e20c3e48e6dc

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
