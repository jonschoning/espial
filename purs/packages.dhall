let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.5-20191227/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.5-20191227/src/packages.dhall sha256:dee386e102d6b088702ceb41d5568b00f28ea3726f8c60378979a6c8b9c37827

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
