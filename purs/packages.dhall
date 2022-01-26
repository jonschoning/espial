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
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.14.5-20220110/src/packages.dhall sha256:8dbf71bfc6c7a11043619eebe90ff85f7d884541048aa8cc48eef1ee781cbc0e

in  upstream
