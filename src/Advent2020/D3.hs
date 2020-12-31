module Advent2020.D3 (run, part1, part2) where

import Advent2020.Internal (Grid, simpleRun)
import Advent2020.Internal.D3 (Slope (..), Square, parse, treesPerSlope)
import Relude

run :: (Grid Square -> Either Text Int) -> Text -> Either Text Int
run = simpleRun parse

part1 :: Grid Square -> Either Text Int
part1 = flip treesPerSlope Slope {right = 3, down = 1}

part2 :: Grid Square -> Either Text Int
part2 smap = product <$> sequence (treesPerSlope smap <$> slopes)
  where
    slopes =
      [ Slope {right = 1, down = 1},
        Slope {right = 3, down = 1},
        Slope {right = 5, down = 1},
        Slope {right = 7, down = 1},
        Slope {right = 1, down = 2}
      ]
