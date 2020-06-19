{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "colors"
  , "concur-vdom"
  , "console"
  , "dom-filereader"
  , "effect"
  , "fft"
  , "halogen-vdom"
  , "numbers"
  , "parsing"
  , "prng-middle-square"
  , "psci-support"
  , "rationals"
  , "sparse-matrices"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
