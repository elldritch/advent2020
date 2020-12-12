module Advent2020.D12 (run, part1) where

import Advent2020.Internal.D12 (Instruction, Ship (..), initial, parse, step)
import Relude

run :: ([Instruction] -> Int) -> Text -> Either Text Int
run runner contents = do
  instructions <- parse contents
  return $ runner instructions

part1 :: [Instruction] -> Int
part1 instructions = let Ship {position = (x, y)} = foldl' step initial instructions in abs x + abs y
