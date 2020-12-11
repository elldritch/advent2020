module Advent2020.D10Spec (spec) where

import Advent2020.D10 (part2)
import Advent2020.Internal.D10 (joltageDifferences)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: NonEmpty Int
exampleInput =
  16
    :| [ 10,
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

exampleInput2 :: NonEmpty Int
exampleInput2 =
  28
    :| [ 33,
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
    joltageDifferences (toList exampleInput) `shouldBe` (7, 0, 5)
    joltageDifferences (toList exampleInput2) `shouldBe` (22, 0, 10)

  it "calculates valid arrangements of adapters" $ do
    part2 exampleInput `shouldBe'` 8
    part2 exampleInput2 `shouldBe'` 19208
