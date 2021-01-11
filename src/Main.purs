module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Console as Console
import Input (Instruction(..), input)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

-- nop +0
-- acc +1
-- jmp +4
-- acc +3
-- jmp -3
-- acc -99
-- acc +1
-- jmp -4
-- acc +6

type Program = Array Instruction

type ProgramState =
  { program :: Program
  , program_counter :: Int
  , accumulator :: Int
  , seen :: Set Int
  }

input_small :: Program
input_small = [Nop 0, Acc 1, Jmp 4, Acc 3, Jmp (-3), Acc (-99), Acc 1, Jmp (-4), Acc 6]

initialState :: ProgramState
initialState =
  { program: input
  , program_counter: 0
  , accumulator: 0
  , seen: Set.empty
  }

step :: ProgramState -> Either Int ProgramState
step st@{ program, program_counter, accumulator, seen } =
  if Set.member program_counter seen then
    Left accumulator
  else
    case Array.index program program_counter of
      Just (Nop _) ->
        Right
          (st
          { program_counter = st.program_counter + 1
          , seen = Set.insert program_counter st.seen
          })
      Just (Acc x) ->
        Right
          (st
          { program_counter = st.program_counter + 1
          , seen = Set.insert program_counter st.seen
          , accumulator = st.accumulator + x
          })
      Just (Jmp x) ->
        Right
          (st
          { program_counter = st.program_counter + x
          , seen = Set.insert program_counter st.seen
          })
      Nothing ->
          Left accumulator

eval :: ProgramState -> Int
eval st = case step st of
  Right nextState -> eval nextState
  Left accumulator -> accumulator

main :: Effect Unit
main = do
  contents <- FS.readTextFile UTF8 "input.txt"
  Console.log contents
