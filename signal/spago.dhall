{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "dom-filereader"
  , "effect"
  , "psci-support"
  , "rationals"
  , "spork"
  , "prng-middle-square"
  , "cartesian"
  , "fft"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
