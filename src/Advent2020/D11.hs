module Advent2020.D11 (run, part1, part2) where

import Advent2020.Internal (fixed, gridMap, simpleRun')
import Advent2020.Internal.D11 (Grid (..), Square (..), adjacent, firstVisibleSeat, parse, step)
import Control.Lens (view)
import Relude

run :: (Grid Square -> Grid Square) -> Text -> Either Text Int
run = simpleRun' parse (return . length . filter (== Occupied) . toList . view gridMap)

part1 :: Grid Square -> Grid Square
part1 = fixed (step adjacent 4)

part2 :: Grid Square -> Grid Square
part2 = fixed (step firstVisibleSeat 5)
