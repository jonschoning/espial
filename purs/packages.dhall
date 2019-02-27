let mkPackage =
      https://raw.githubusercontent.com/spacchetti/spacchetti/0.12.3-20190226/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/spacchetti/spacchetti/0.12.3-20190226/src/packages.dhall sha256:832321319d21051fe1c0ff21bcee77af1f86bf7700d2041e1e1c1ac6b1dc4ea1

let overrides = {=}

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
