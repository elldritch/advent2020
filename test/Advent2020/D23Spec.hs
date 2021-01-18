module Advent2020.D23Spec (spec) where

import Advent2020.Internal (unsafeNonEmpty)
import Advent2020.Internal.D23 (Cups, makeCups, parse, step)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: Text
exampleInput = unlines ["389125467"]

exampleCups :: Cups
exampleCups = makeCups [3, 8, 9, 1, 2, 5, 4, 6, 7]

spec :: Spec
spec = do
  it "parses cups" $ do
    parse exampleInput `shouldBe'` exampleCups

  it "makes cup moves" $ do
    head (unsafeNonEmpty $ drop 10 $ iterate step exampleCups) `shouldBe` makeCups [8, 3, 7, 4, 1, 9, 2, 6, 5]
