module Advent2020.D5 (run, part1, part2) where

import Advent2020.Internal (largest, simpleRun, windows)
import Advent2020.Internal.D5 (Seat, SeatPosition (..), parse, parseSpec, seatID)
import Relude
import Relude.Extra.Map

run :: (NonEmpty Seat -> Either Text t) -> Text -> Either Text t
run = simpleRun (parse >=> mapM parseSpec)

part1 :: NonEmpty Seat -> Either Text Int
part1 = return . largest . fmap seatID

part2 :: NonEmpty Seat -> Either Text Int
part2 ps = seatID <$> maybeToRight "could not find open seat" openSeat
  where
    occupied :: Set Seat
    occupied = fromList $ toList ps

    isOccupied :: Seat -> Bool
    isOccupied p = member p occupied

    seats :: [Seat]
    seats = sortWith row $ sortWith column $ [SeatPosition {..} | column <- [0 .. 7], row <- [0 .. 127]]

    isOpen :: [Seat] -> Bool
    isOpen [a, b, c] = isOccupied a && not (isOccupied b) && isOccupied c
    isOpen xs = error $ "impossible: window of length " <> show (length xs)

    openSeat :: Maybe Seat
    openSeat = do
      [_, b, _] <- find isOpen $ windows 3 seats
      return b
