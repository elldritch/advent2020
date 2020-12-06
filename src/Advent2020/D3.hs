module Advent2020.D3 (run, part1, part2) where

import Advent2020.Internal (gather', label)
import Advent2020.Internal.D3 (SledMap, Slope (..), parse, treesPerSlope)
import Relude

run :: Text -> (SledMap -> Either Text Int) -> Either Text Int
run contents runner = do
  smap <- label "parsing sled map" $ parse contents
  label "computing trees in slopes" $ runner smap

part1 :: SledMap -> Either Text Int
part1 = flip treesPerSlope Slope {right = 3, down = 1}

part2 :: SledMap -> Either Text Int
part2 smap = do
  trees <- gather' $ treesPerSlope smap <$> slopes
  return $ product trees
  where
    slopes =
      [ Slope {right = 1, down = 1},
        Slope {right = 3, down = 1},
        Slope {right = 5, down = 1},
        Slope {right = 7, down = 1},
        Slope {right = 1, down = 2}
      ]
