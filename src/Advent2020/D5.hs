module Advent2020.D5 (run, part1) where

import Advent2020.Internal (gather')
import Advent2020.Internal.D5 (Position, parse, seatID, specToPosition)
import Relude

run :: Text -> ([Position Int] -> Either Text t) -> Either Text t
run contents runner = do
  specs <- parse contents
  ps <- gather' $ specToPosition <$> specs
  runner ps

part1 :: [Position Int] -> Either Text Int
part1 ps = return $ foldr max 0 $ seatID <$> ps
