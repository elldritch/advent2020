module Advent2020.D8 (run, part1, part2) where

import Advent2020.Internal (setAt, simpleRun)
import Advent2020.Internal.D8 (Instruction (..), Machine (..), Operation (..), Program, Status (..), parse, runUntilFixed)
import Control.Monad.Extra (findM)
import Relude

run :: (Program -> Either Text Int) -> Text -> Either Text Int
run = simpleRun parse

part1 :: Program -> Either Text Int
part1 program = do
  (_, Machine {..}) <- runUntilFixed program
  return accumulator

part2 :: Program -> Either Text Int
part2 program = do
  programs <- mconcat <$> sequence (changeProgram' program <$> [1 .. (length program - 1)])
  terminates <- maybeToRight "could not find terminating program" =<< findM programTerminates programs
  (_, Machine {..}) <- runUntilFixed terminates
  return accumulator
  where
    changeProgram' :: Program -> Int -> Either Text [Program]
    changeProgram' p n = do
      i@Instruction {..} <- maybeToRight "instruction index out-of-bounds" $ p !!? n
      let i' = changeInstruction i
      sequence $ (\v -> setAt n v p) <$> i'

    changeInstruction :: Instruction -> [Instruction]
    changeInstruction i@Instruction {..} = case operation of
      NoOp -> [i, i {operation = Jump}]
      Jump -> [i, i {operation = NoOp}]
      Accumulate -> [i]

    programTerminates :: Program -> Either Text Bool
    programTerminates p = do
      (_, Machine {..}) <- runUntilFixed p
      return $ case status of
        Terminated -> True
        _ -> False
