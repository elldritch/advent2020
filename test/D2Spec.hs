module D2Spec (spec) where

import Advent2020.D2 (part1, part2)
import Advent2020.Internal.D2 (Password (..), errorBundlePretty, parse)
import Relude
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

spec :: Spec
spec = do
  let exampleInput = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n"
  let exampleParse =
        [ Password {a = 1, b = 3, letter = 'a', password = "abcde"},
          Password {a = 1, b = 3, letter = 'b', password = "cdefg"},
          Password {a = 2, b = 9, letter = 'c', password = "ccccccccc"}
        ]

  describe "part 1" $ do
    it "parses password files" $ do
      let parsed = parse exampleInput
      case parsed of
        Right passwords -> passwords `shouldBe` exampleParse
        Left errors -> expectationFailure $ errorBundlePretty errors
    it "determines valid passwords" $ do
      fmap part1 exampleParse `shouldBe` [True, False, True]

  describe "part 2" $ do
    it "determines Toboggan-valid passwords" $ do
      fmap part2 exampleParse `shouldBe` [True, False, False]
