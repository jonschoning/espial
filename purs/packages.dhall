let mkPackage =
	  https://raw.githubusercontent.com/spacchetti/spacchetti/0.12.3-20190226/src/mkPackage.dhall

let upstream =
	  https://raw.githubusercontent.com/spacchetti/spacchetti/0.12.3-20190226/src/packages.dhall

let overrides =
	  { halogen =
		  upstream.halogen ⫽ { version = "v5.0.0-rc.1" }
	  , halogen-vdom =
		  upstream.halogen-vdom ⫽ { version = "v5.1.0" }
	  }

let additions = {=}

in  upstream ⫽ overrides ⫽ additions