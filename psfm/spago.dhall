{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "cartesian"
  , "console"
  , "effect"
  , "integers"
  , "parsing"
  , "prng-middle-square"
  , "psci-support"
  , "sparse-polynomials"
  , "spork"
  , "strings"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
