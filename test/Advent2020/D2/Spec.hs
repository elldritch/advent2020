module Advent2020.D2.Spec (spec) where

import Advent2020.D2 (part1, part2)
import Advent2020.Internal.D2 (Password (..), parse)
import Relude
import Test.Hspec (Spec, expectationFailure, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ "1-3 a: abcde",
      "1-3 b: cdefg",
      "2-9 c: ccccccccc"
    ]

examplePasswords :: [Password]
examplePasswords =
  [ Password {a = 1, b = 3, letter = 'a', password = "abcde"},
    Password {a = 1, b = 3, letter = 'b', password = "cdefg"},
    Password {a = 2, b = 9, letter = 'c', password = "ccccccccc"}
  ]

spec :: Spec
spec = do
  it "parses password files" $ do
    let parsed = parse exampleInput
    case parsed of
      Right passwords -> passwords `shouldBe` examplePasswords
      Left errors -> expectationFailure $ toString errors

  it "determines valid passwords" $ do
    part1 <$> examplePasswords `shouldBe` Right <$> [True, False, True]

  it "determines Toboggan-valid passwords" $ do
    part2 <$> examplePasswords `shouldBe` Right <$> [True, False, False]
