{-
Welcome to Spacchetti local packages!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
	default package set's release
- Use your own modified version of some dependency that may
	include new API, changed API, removed API by
	using your custom git repo of the library rather than
	the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
	  upstream.packageName ⫽ { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
	  upstream.packageName ⫽ { version = "v4.0.0" }
  , packageName =
	  upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
	  upstream.halogen ⫽ { version = "master" }
  , halogen-vdom =
	  upstream.halogen-vdom ⫽ { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't alread included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { "package-name" =
	   mkPackage
		 [ "dependency1"
		 , "dependency2"
		 ]
		 "https://example.com/path/to/git/repo.git"
		 "tag ('v4.0.0') or branch ('master')"
  , "package-name" =
	   mkPackage
		 [ "dependency1"
		 , "dependency2"
		 ]
		 "https://example.com/path/to/git/repo.git"
		 "tag ('v4.0.0') or branch ('master')"
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
	  mkPackage
		[ "arrays"
		, "exists"
		, "profunctor"
		, "strings"
		, "quickcheck"
		, "lcg"
		, "transformers"
		, "foldable-traversable"
		, "exceptions"
		, "node-fs"
		, "node-buffer"
		, "node-readline"
		, "datetime"
		, "now"
		]
		"https://github.com/hdgarrood/purescript-benchotron.git"
		"v7.0.0"
  }
-------------------------------
-}

let mkPackage =
      https://raw.githubusercontent.com/spacchetti/spacchetti/20190105/src/mkPackage.dhall sha256:8e1c6636f8a089f972b21cde0cef4b33fa36a2e503ad4c77928aabf92d2d4ec9

let upstream =
      https://raw.githubusercontent.com/spacchetti/spacchetti/20190105/src/packages.dhall sha256:38fc3e19c193bb006c773ac84fc4a2888e5dcc610d36e49a9bdef7ecc7e1f8c9

let overrides = {=}

let additions = {=}

in  upstream ⫽ overrides ⫽ additions