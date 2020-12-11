module Advent2020.Internal.D10 (joltageDifferences) where

import Advent2020.Internal (windows)
import Relude

joltageDifferences :: [Int] -> (Int, Int, Int)
joltageDifferences ns = foldr countDifferences (0, 0, 1) deltas -- There is always a 3-jolt jump to the laptop
  where
    deltas = windows 2 $ sort $ 0 : ns -- Charger always starts at 0 jolts
    countDifferences [a, b] acc@(ones, twos, threes) = case b - a of
      3 -> (ones, twos, threes + 1)
      1 -> (ones + 1, twos, threes)
      _ -> acc
    countDifferences _ acc = acc
