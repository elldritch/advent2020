module Advent2020.D15Spec (spec) where

import Advent2020.Internal.D15 (nth, spoken)
import Relude
import Test.Hspec (Spec, it, shouldBe, xit)

spec :: Spec
spec = do
  it "gets the 2020th word spoken" $ do
    nth 2020 (spoken [1, 3, 2]) `shouldBe` 1
    nth 2020 (spoken [2, 1, 3]) `shouldBe` 10
    nth 2020 (spoken [1, 2, 3]) `shouldBe` 27
    nth 2020 (spoken [2, 3, 1]) `shouldBe` 78
    nth 2020 (spoken [3, 2, 1]) `shouldBe` 438
    nth 2020 (spoken [3, 1, 2]) `shouldBe` 1836

  xit "gets the 30000000th word spoken" $ do
    nth 30000000 (spoken [0, 3, 6]) `shouldBe` 175594
    nth 30000000 (spoken [1, 3, 2]) `shouldBe` 2578
    nth 30000000 (spoken [2, 1, 3]) `shouldBe` 3544142
    nth 30000000 (spoken [1, 2, 3]) `shouldBe` 261214
    nth 30000000 (spoken [2, 3, 1]) `shouldBe` 6895259
    nth 30000000 (spoken [3, 2, 1]) `shouldBe` 18
    nth 30000000 (spoken [3, 1, 2]) `shouldBe` 362
