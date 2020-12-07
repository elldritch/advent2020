module Advent2020.D1Spec (spec) where

import Advent2020.D1 (part1, part2)
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: [Int]
exampleInput = [1721, 979, 366, 299, 675, 1456]

spec :: Spec
spec = do
  it "finds the product of 2 entries that sum to 2020" $ do
    part1 exampleInput `shouldBe` Just 514579

  it "finds the product of 3 entries that sum to 2020" $ do
    part2 exampleInput `shouldBe` Just 241861950
