let mkPackage =
	  https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.3-20190403/src/mkPackage.dhall

let upstream =
	  https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.3-20190403/src/packages.dhall

let overrides =
	  { halogen =
		  upstream.halogen ⫽ { version = "v5.0.0-rc.4" }
	  , halogen-vdom =
		  upstream.halogen-vdom ⫽ { version = "v6.1.0" }
	  }

let additions = {=}

in  upstream ⫽ overrides ⫽ additions