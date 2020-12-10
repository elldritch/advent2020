module Advent2020.D5 (run, part1, part2) where

import Advent2020.Internal (gather')
import Advent2020.Internal.D5 (Position (..), parse, seatID, specToPosition)
import Data.Set (member)
import Relude

run :: Text -> ([Position Int] -> Either Text t) -> Either Text t
run contents runner = do
  specs <- parse contents
  ps <- gather' $ specToPosition <$> specs
  runner ps

part1 :: [Position Int] -> Either Text Int
part1 ps = return $ foldr max 0 $ seatID <$> ps

part2 :: [Position Int] -> Either Text Int
part2 ps = do
  let x = viaNonEmpty head $ filter (\Position{..} -> row > 10 && row < 100) missing
  seatID <$> maybeToRight "could not find open seat" x
  where
    occupied :: Set (Position Int)
    occupied = fromList ps

    seats :: [Position Int]
    seats = sortWith row $ sortWith column $ [Position {..} | column <- [0 .. 7], row <- [0 .. 127]]

    missing :: [Position Int]
    missing = filter (\p -> not $ member p occupied) seats
