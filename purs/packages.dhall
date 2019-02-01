let mkPackage =
      https://raw.githubusercontent.com/spacchetti/spacchetti/20190131/src/mkPackage.dhall sha256:8e1c6636f8a089f972b21cde0cef4b33fa36a2e503ad4c77928aabf92d2d4ec9

let upstream =
      https://raw.githubusercontent.com/spacchetti/spacchetti/20190131/src/packages.dhall sha256:b08ac97c4447bd49d5c5fef40d95ff181def4a84672f84b79035d01e59d53905

let overrides = {=}

let additions = {=}

in  upstream ⫽ overrides ⫽ additions