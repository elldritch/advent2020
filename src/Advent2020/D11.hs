module Advent2020.D11 (run, part1) where

import Advent2020.Internal (fixed)
import Advent2020.Internal.D11 (Grid, Position (..), parse, step)
import Relude

run :: (Grid -> Either Text Int) -> Text -> Either Text Int
run runner contents = do
  grid <- parse contents
  runner grid

part1 :: Grid -> Either Text Int
part1 grid = return $ length $ filter (== Occupied) $ toList $ fixed step grid
