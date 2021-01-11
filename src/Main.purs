module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

main :: Effect Unit
main = do
  contents <- FS.readTextFile UTF8 "input.txt"
  Console.log contents
