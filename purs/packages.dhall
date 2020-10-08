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
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.8-20200922/src/packages.dhall sha256:5edc9af74593eab8834d7e324e5868a3d258bbab75c5531d2eb770d4324a2900
in upstream
