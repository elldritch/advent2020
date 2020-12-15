module Advent2020.D15Spec (spec) where

import Advent2020.Internal.D15 (Game (..), spoken)
import Relude
import Relude.Unsafe (fromJust)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "gets the 2020th word spoken" $ do
    next (fromJust $ find ((== 2020) . currentTurn) $ spoken [1, 3, 2]) `shouldBe` 1
    next (fromJust $ find ((== 2020) . currentTurn) $ spoken [2, 1, 3]) `shouldBe` 10
    next (fromJust $ find ((== 2020) . currentTurn) $ spoken [1, 2, 3]) `shouldBe` 27
    next (fromJust $ find ((== 2020) . currentTurn) $ spoken [2, 3, 1]) `shouldBe` 78
    next (fromJust $ find ((== 2020) . currentTurn) $ spoken [3, 2, 1]) `shouldBe` 438
    next (fromJust $ find ((== 2020) . currentTurn) $ spoken [3, 1, 2]) `shouldBe` 1836
