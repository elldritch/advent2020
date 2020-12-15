module Advent2020.D14 (run, part1, part2) where

import Advent2020.Internal (simpleRun')
import Advent2020.Internal.D14 (BitMask, DecodeMask, Machine (..), Program, execute, parse, step, step')
import Relude

run :: (Program -> Machine m) -> Text -> Either Text Integer
run = simpleRun' parse (return . sum . memory)

part1 :: Program -> Machine BitMask
part1 = execute step

part2 :: Program -> Machine DecodeMask
part2 = execute step'
