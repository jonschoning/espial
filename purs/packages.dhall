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
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.13-20231219/src/packages.dhall
        sha256:35b9271b0a49390a9681995c609dbf7357402a1f209e0549d840bca295abe57b

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
