module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Console as Console
import Input (Instruction(..))
import Input as Input

type Program = Array Instruction

type ProgramState =
  { program :: Program
  , program_counter :: Int
  , accumulator :: Int
  , seen :: Set Int
  }

input_small :: Program
input_small = [Nop 0, Acc 1, Jmp 4, Acc 3, Jmp (-3), Acc (-99), Acc 1, Nop (-4), Acc 6]

initialState :: ProgramState
initialState =
--  { program: input_small
  { program: Input.input
  , program_counter: 0
  , accumulator: 0
  , seen: Set.empty
  }

data StepResult
  = Loop Int
  | Terminate Int
  | Next ProgramState

step :: ProgramState -> StepResult
step old_state@{ program, program_counter, accumulator, seen } =
  if Set.member program_counter seen then
    Loop accumulator
  else
    let st = old_state { seen = Set.insert program_counter seen } in
    case Array.index program program_counter of
      Just (Nop _) ->
        Next (st { program_counter = program_counter + 1 })
      Just (Acc x) ->
        Next (st
          { program_counter = program_counter + 1
          , accumulator = accumulator + x
          })
      Just (Jmp x) ->
        Next (st { program_counter = program_counter + x })
      Nothing ->
        Terminate accumulator

data EvalResult = Looped Int | Terminated Int

eval :: ProgramState -> EvalResult
eval st = case step st of
  Next nextState -> eval nextState
  Loop accumulator -> Looped accumulator
  Terminate accumulator -> Terminated accumulator

flipInstruction :: ProgramState -> Maybe ProgramState
flipInstruction st = do
  current_instruction <- Array.index st.program st.program_counter
  flipped <- case current_instruction of
    Jmp x -> Just (Nop x)
    Nop x -> Just (Jmp x)
    Acc x -> Nothing
  new_program <- Array.updateAt st.program_counter flipped st.program
  Just (st { program = new_program })

solve1 :: ProgramState -> Either String Int
solve1 st = case eval st of
  Looped x -> 
    Right x
  Terminated x ->
    Left "Accidentally correct"

solve2 :: ProgramState -> Either String Int
solve2 st = 
  case flipInstruction st of
    Just flipped 
      | Terminated x <- eval flipped -> Right x
    _ -> case step st of
      Next nextState ->
        solve2 nextState
      Loop accumulator ->
        Left "Not fixable by changing a single instruction"
      Terminate accumulator ->
        Left "Accidentally correct"

main :: Effect Unit
main = do
  Console.log "Problem 1:"
  Console.logShow (solve1 initialState)
  Console.log "Problem 2:"
  Console.logShow (solve2 initialState)
