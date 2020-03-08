let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.6-20200226/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.6-20200226/src/packages.dhall sha256:3a52562e05b31a7b51d12d5b228ccbe567c527781a88e9028ab42374ab55c0f1

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
