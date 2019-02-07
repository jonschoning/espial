let mkPackage =
      https://raw.githubusercontent.com/spacchetti/spacchetti/0.12.2-20190209/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/spacchetti/spacchetti/0.12.2-20190209/src/packages.dhall sha256:e330a01c5c503c5dfccda6c38282ef03d10005a67291b3f82ddcfca544ad8bc8

let overrides = {=}

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
