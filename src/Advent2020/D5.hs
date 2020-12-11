module Advent2020.D5 (run, part1, part2) where

import Advent2020.Internal (gather', windows)
import Advent2020.Internal.D5 (Position (..), parse, seatID, specToPosition)
import Data.Set (member)
import Relude

run :: ([Position Int] -> Either Text t) -> Text -> Either Text t
run runner contents = do
  specs <- parse contents
  ps <- gather' $ specToPosition <$> specs
  runner ps

part1 :: [Position Int] -> Either Text Int
part1 ps = return $ foldr max 0 $ seatID <$> ps

part2 :: [Position Int] -> Either Text Int
part2 ps = seatID <$> maybeToRight "could not find open seat" openSeat
  where
    occupied :: Set (Position Int)
    occupied = fromList ps

    isOccupied :: Position Int -> Bool
    isOccupied p = member p occupied

    seats :: [Position Int]
    seats = sortWith row $ sortWith column $ [Position {..} | column <- [0 .. 7], row <- [0 .. 127]]

    isOpen :: [Position Int] -> Bool
    isOpen [a, b, c] = isOccupied a && not (isOccupied b) && isOccupied c
    isOpen xs = error $ "impossible: window of length " <> show (length xs)

    openSeat :: Maybe (Position Int)
    openSeat = do
      [_, b, _] <- find isOpen $ windows 3 seats
      return b
