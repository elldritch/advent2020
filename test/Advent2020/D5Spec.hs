module Advent2020.D5Spec (spec) where

import Advent2020.Internal.D5 (Partition (..), Seat, SeatPosition (..), SeatSpec, parse, parseSpec, seatID)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ "FBFBBFFRLR",
      "BFFFBBFRRR",
      "FFFBBBFRRR",
      "BBFFBBFRLL"
    ]

exampleSeatSpecs :: NonEmpty SeatSpec
exampleSeatSpecs =
  [F, B, F, B, B, F, F, R, L, R]
    :| [ [B, F, F, F, B, B, F, R, R, R],
         [F, F, F, B, B, B, F, R, R, R],
         [B, B, F, F, B, B, F, R, L, L]
       ]

exampleSeatPositions :: [Seat]
exampleSeatPositions =
  [ SeatPosition {row = 44, column = 5},
    SeatPosition {row = 70, column = 7},
    SeatPosition {row = 14, column = 7},
    SeatPosition {row = 102, column = 4}
  ]

exampleSeatIDs :: [Int]
exampleSeatIDs = [357, 567, 119, 820]

spec :: Spec
spec = do
  it "parses seat specs" $ do
    parse exampleInput `shouldBe'` exampleSeatSpecs

  it "computes seat positions from specs" $ do
    sequence (toList $ parseSpec <$> exampleSeatSpecs) `shouldBe'` exampleSeatPositions

  it "computes seat IDs from positions" $ do
    seatID <$> exampleSeatPositions `shouldBe` exampleSeatIDs
