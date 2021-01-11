{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "aoc-2020"
, dependencies =
  [ "console", "effect", "node-fs", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
