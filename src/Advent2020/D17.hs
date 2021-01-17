module Advent2020.D17 (run, part1, part2) where

import Advent2020.Internal.D17 (Cube, Hypercube, Pocket (..), numCubes, parse, parse', stepN, stepN')
import Relude

run :: (Ord t) => (Text -> Either Text (Pocket t), Int -> Pocket t -> Pocket t) -> Text -> Either Text Int
run (parser, stepper) contents = parser contents <&> numCubes . stepper 6

part1 :: (Text -> Either Text (Pocket Cube), Int -> Pocket Cube -> Pocket Cube)
part1 = (parse, stepN)

part2 :: (Text -> Either Text (Pocket Hypercube), Int -> Pocket Hypercube -> Pocket Hypercube)
part2 = (parse', stepN')
