let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190614/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190614/src/packages.dhall sha256:53f95298ca7734c037a0ebfd2ce982c004d8377ebc01cc3387f5a61508c6b8ac

let overrides = {=}

let overrides =
      { halogen =
          upstream.halogen ⫽ { version = "v5.0.0-rc.4" }
      , halogen-vdom =
          upstream.halogen-vdom ⫽ { version = "v6.1.0" }
      }

let additions = {=}

in  upstream // overrides // additions
