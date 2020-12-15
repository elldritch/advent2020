module Advent2020.D15 (run, part1, part2) where

import Advent2020.Internal (simpleRun')
import Advent2020.Internal.D15 (nth, parse, spoken)
import Relude

run :: ([Integer] -> Integer) -> Text -> Either Text Integer
run = simpleRun' parse return

part1 :: [Integer] -> Integer
part1 = nth 2020 . spoken

part2 :: [Integer] -> Integer
part2 = nth 30000000 . spoken
