module Advent2020.D9 (run, part1, part2) where

import Advent2020.Internal (runNumbers)
import Advent2020.Internal.D9 (findWeakNumber, findWeakSet)
import Relude

run :: ([Int] -> Either Text Int) -> Text -> Either Text Int
run = runNumbers

part1 :: [Int] -> Either Text Int
part1 = findWeakNumber 25

part2 :: [Int] -> Either Text Int
part2 = findWeakSet 25
