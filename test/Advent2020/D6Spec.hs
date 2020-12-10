module Advent2020.D6Spec (spec) where

import Advent2020.D6 (part1)
import Advent2020.Internal.D6 (Group, parse)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it)

exampleInput :: Text
exampleInput =
  unlines
    [ "abc",
      "",
      "a",
      "b",
      "c",
      "",
      "ab",
      "ac",
      "",
      "a",
      "a",
      "a",
      "a",
      "",
      "b"
    ]

exampleGroups :: [Group]
exampleGroups =
  [ [fromList ['a', 'b', 'c']],
    [one 'a', one 'b', one 'c'],
    [fromList ['a', 'b'], fromList ['a', 'c']],
    [one 'a', one 'a', one 'a', one 'a'],
    [one 'b']
  ]

spec :: Spec
spec = do
  it "parses response groups" $ do
    parse exampleInput `shouldBe'` exampleGroups

  it "sums questions answered per group" $ do
    part1 exampleInput `shouldBe'` 11
