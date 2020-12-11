module Advent2020.D10Spec (spec) where

import Advent2020.Internal.D10 (joltageDifferences)
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: [Int]
exampleInput =
  [ 16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4
  ]

exampleInput2 :: [Int]
exampleInput2 =
  [ 28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3
  ]

spec :: Spec
spec = do
  it "calculates joltage difference distributions" $ do
    joltageDifferences exampleInput `shouldBe` (7, 0, 5)
    joltageDifferences exampleInput2 `shouldBe` (22, 0, 10)
