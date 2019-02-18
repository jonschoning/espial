let mkPackage =
      https://raw.githubusercontent.com/spacchetti/spacchetti/0.12.2-20190210/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/spacchetti/spacchetti/0.12.2-20190210/src/packages.dhall sha256:1bee3f7608ca0f87a88b4b8807cb6722ab9ce3386b68325fbfa71d7211c1cf51

let overrides = {=}

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
