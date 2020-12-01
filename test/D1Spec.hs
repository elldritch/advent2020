module D1Spec (spec) where

import Advent2020.D1 (part1)
import Relude
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "finds the product of entries that sum to 2020" $ do
    let exampleInput = [1721, 979, 366, 299, 675, 1456]
    part1 exampleInput `shouldBe` 514579
