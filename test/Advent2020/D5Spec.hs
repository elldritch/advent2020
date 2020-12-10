module Advent2020.D5Spec (spec) where

import Advent2020.D5 (part1, Partition (..), SeatSpec, parse)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it)

exampleInput :: Text
exampleInput =
  unlines
    [ "FBFBBFFRLR",
      "BFFFBBFRRR",
      "FFFBBBFRRR",
      "BBFFBBFRLL"
    ]

exampleSeatSpecs :: [SeatSpec]
exampleSeatSpecs =
  [ [F, B, F, B, B, F, F, R, L, R],
    [B, F, F, F, B, B, F, R, R, R],
    [F, F, F, B, B, B, F, R, R, R],
    [B, B, F, F, B, B, F, R, L, L]
  ]

spec :: Spec
spec = do
  it "parses seat specs" $ do
    parse exampleInput `shouldBe'` exampleSeatSpecs
  it "finds the maximum seat id from a list of seat specs" $ do
    part1 exampleInput `shouldBe'` 820

