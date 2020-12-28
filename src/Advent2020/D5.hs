module Advent2020.D5 (run, part1, part2) where

import Advent2020.Internal (largest, simpleRun, windows)
import Advent2020.Internal.D5 (Position (..), parse, seatID, specToPosition)
import Relude
import Relude.Extra.Map

run :: (NonEmpty (Position Int) -> Either Text t) -> Text -> Either Text t
run = simpleRun (parse >=> mapM specToPosition)

part1 :: NonEmpty (Position Int) -> Either Text Int
part1 ps = return $ largest $ seatID <$> ps

part2 :: NonEmpty (Position Int) -> Either Text Int
part2 ps = seatID <$> maybeToRight "could not find open seat" openSeat
  where
    occupied :: Set (Position Int)
    occupied = fromList $ toList ps

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
