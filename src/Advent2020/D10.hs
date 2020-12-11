module Advent2020.D10 (run, part1) where

import Advent2020.Internal (runNumbers)
import Advent2020.Internal.D10 (joltageDifferences)
import Relude

run :: ([Int] -> Either Text Int) -> Text -> Either Text Int
run = runNumbers

part1 :: [Int] -> Either Text Int
part1 ns = let (a, _, c) = joltageDifferences ns in return $ a * c
