{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "cartesian"
  , "colors"
  , "console"
  , "dom-filereader"
  , "effect"
  , "fft"
  , "integers"
  , "lists"
  , "numbers"
  , "parsing"
  , "prng-middle-square"
  , "psci-support"
  , "sparse-polynomials"
  , "spork"
  , "strings"
  , "variant"
  , "web-file"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
