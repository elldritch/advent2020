module Advent2020.D11 (run, part1, part2) where

import Advent2020.Internal (fixed, simpleRun')
import Advent2020.Internal.D11 (Grid (..), Position (..), adjacent, firstVisibleSeat, parse, step)
import Relude

run :: (Grid -> Grid) -> Text -> Either Text Int
run = simpleRun' parse (return . length . filter (== Occupied) . toList . grid)

part1 :: Grid -> Grid
part1 = fixed (step adjacent 4)

part2 :: Grid -> Grid
part2 = fixed (step firstVisibleSeat 5)
