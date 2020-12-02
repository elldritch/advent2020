module D1Spec (spec) where

import Advent2020.D1 (part2, part1)
import Relude
import Test.Hspec (describe, Spec, it, shouldBe)

spec :: Spec
spec = do
  let exampleInput = [1721, 979, 366, 299, 675, 1456]
  describe "part 1" $ do
    it "finds the product of 2 entries that sum to 2020" $ do
      part1 exampleInput `shouldBe` 514579

  describe "part 2" $ do
    it "finds the product of 3 entries that sum to 2020" $ do
      part2 exampleInput `shouldBe` 241861950