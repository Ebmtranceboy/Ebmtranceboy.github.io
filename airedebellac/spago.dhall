{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "datetime"
  , "debug"
  , "effect"
  , "event"
  , "halogen"
  , "psci-support"
  , "simple-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
