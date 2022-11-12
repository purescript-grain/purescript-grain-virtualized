{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "grain-virtualized"
, license = "MIT"
, repository =
    "https://github.com/purescript-grain/purescript-grain-virtualized"
, dependencies =
  [ "arrays"
  , "foldable-traversable"
  , "grain"
  , "maybe"
  , "prelude"
  , "web-dom"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
