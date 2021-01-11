module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Console as Console
import Input (Instruction(..))

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

input_small1 :: Program
input_small1 = [Nop 0]

initialState :: ProgramState
initialState =
  { program: input_small1
  , program_counter: 0
  , accumulator: 0
  , seen: Set.empty
  }

data Result
  = Loop Int
  | Terminate Int
  | Next ProgramState

step :: ProgramState -> Result
step st@{ program, program_counter, accumulator, seen } =
  if Set.member program_counter seen then
    Loop accumulator
  else
    case Array.index program program_counter of
      Just (Nop _) ->
        Next
          (st
          { program_counter = st.program_counter + 1
          , seen = Set.insert program_counter st.seen
          })
      Just (Acc x) ->
        Next
          (st
          { program_counter = st.program_counter + 1
          , seen = Set.insert program_counter st.seen
          , accumulator = st.accumulator + x
          })
      Just (Jmp x) ->
        Next
          (st
          { program_counter = st.program_counter + x
          , seen = Set.insert program_counter st.seen
          })
      Nothing ->
        Terminate accumulator

eval :: ProgramState -> Int
eval st = case step st of
  Next nextState -> eval nextState
  Loop accumulator -> accumulator
  Terminate accumulator -> accumulator

eval' :: ProgramState -> Maybe Int
eval' st = case step st of
  Next nextState -> eval' nextState
  Loop accumulator -> Nothing
  Terminate accumulator -> Just accumulator

flipInstruction :: Instruction -> Instruction
flipInstruction = case _ of
  Jmp x -> Nop x
  Nop x -> Jmp x
  Acc x -> Acc x

flip :: ProgramState -> ProgramState
flip st =
  st
  { program =
       fromMaybe st.program (Array.modifyAt st.program_counter flipInstruction st.program)
  }

evalFix :: ProgramState -> Either String Int
evalFix st = case eval' (flip st) of
  Just result ->
    Right result
  Nothing ->
    case step st of
      Next nextState ->
        evalFix nextState
      Loop accumulator ->
        Left "Not fixable by changing a single instruction"
      Terminate accumulator ->
        Left "Accidentally correct"

main :: Effect Unit
main = do
  let result = evalFix initialState
  Console.logShow result
