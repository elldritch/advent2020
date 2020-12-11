module Advent2020.D9Spec (spec) where

import Advent2020.Internal.D9 (findWeakNumber)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it)

exampleInput :: [Int]
exampleInput =
  [ 35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576
  ]

spec :: Spec
spec = do
  it "finds weak XMAS numbers" $ do
    findWeakNumber 5 exampleInput `shouldBe'` 127
    findWeakNumber 25 ([1 .. 25] ++ [100]) `shouldBe'` 100
    findWeakNumber 25 ([1 .. 25] ++ [50]) `shouldBe'` 50
