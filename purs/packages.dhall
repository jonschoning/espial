let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.8-20200831/src/packages.dhall sha256:cdb3529cac2cd8dd780f07c80fd907d5faceae7decfcaa11a12037df68812c83

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
