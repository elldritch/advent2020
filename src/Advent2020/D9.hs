module Advent2020.D9 (run, part1, part2) where

import Advent2020.Internal (parseNumbers)
import Advent2020.Internal.D9 (findWeakNumber, findWeakSet)
import Relude

run :: Text -> ([Int] -> Either Text Int) -> Either Text Int
run contents runner = do
  ns <- parseNumbers contents
  runner ns

part1 :: [Int] -> Either Text Int
part1 = findWeakNumber 25

part2 :: [Int] -> Either Text Int
part2 = findWeakSet 25
