let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.8-20200708/src/packages.dhall sha256:df5b0f1ae92d4401404344f4fb2a7a3089612c9f30066dcddf9eaea4fe780e29

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
