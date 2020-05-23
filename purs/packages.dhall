let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.6-20200507/src/packages.dhall sha256:9c1e8951e721b79de1de551f31ecb5a339e82bbd43300eb5ccfb1bf8cf7bbd62

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
