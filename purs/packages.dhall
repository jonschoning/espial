let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.6-20200404/src/packages.dhall sha256:f239f2e215d0cbd5c203307701748581938f74c4c78f4aeffa32c11c131ef7b6

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
