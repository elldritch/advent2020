module D2Spec (spec) where

import Advent2020.D2 (isValid, errorBundlePretty, Password (..), parse)
import Relude
import Test.Hspec (expectationFailure, Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  let exampleInput = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n"
  let exampleParse =
        [ Password {min = 1, max = 3, letter = 'a', password = "abcde"},
          Password {min = 1, max = 3, letter = 'b', password = "cdefg"},
          Password {min = 2, max = 9, letter = 'c', password = "ccccccccc"}
        ]

  describe "part 1" $ do
    it "parses password files" $ do
      let parsed = parse exampleInput
      case parsed of
        Right passwords -> passwords `shouldBe` exampleParse
        Left errors -> expectationFailure $ errorBundlePretty errors
    it "determines valid passwords" $ do
      fmap isValid exampleParse `shouldBe` [True, False, True]
