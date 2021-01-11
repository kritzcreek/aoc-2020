{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "aoc-2020"
, dependencies =
  [ "console", "effect", "psci-support", "ordered-collections" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
