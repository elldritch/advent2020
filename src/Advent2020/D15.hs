module Advent2020.D15 (run, part1) where

import Advent2020.Internal (simpleRun')
import Advent2020.Internal.D15 (Game (..), parse, spoken)
import Relude
import Relude.Unsafe (fromJust)

run :: ([Int] -> Int) -> Text -> Either Text Int
run = simpleRun' parse return

part1 :: [Int] -> Int
part1 = next . fromJust . find ((2020 ==) . currentTurn) . spoken
