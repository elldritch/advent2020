module Advent2020.D3Spec (spec) where

import Advent2020.Internal (Grid (..))
import Advent2020.Internal.D3 (Slope (..), Square (..), parse, treesPerSlope)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ "..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"
    ]

exampleSledMap :: Grid Square
exampleSledMap =
  Grid
    { _gridHeight = 11,
      _gridWidth = 11,
      _gridMap =
        fromList
          [ ((0, 0), Open),
            ((0, 1), Tree),
            ((0, 2), Open),
            ((0, 3), Open),
            ((0, 4), Open),
            ((0, 5), Open),
            ((0, 6), Open),
            ((0, 7), Open),
            ((0, 8), Tree),
            ((0, 9), Tree),
            ((0, 10), Open),
            ((1, 0), Open),
            ((1, 1), Open),
            ((1, 2), Tree),
            ((1, 3), Open),
            ((1, 4), Tree),
            ((1, 5), Open),
            ((1, 6), Tree),
            ((1, 7), Tree),
            ((1, 8), Open),
            ((1, 9), Open),
            ((1, 10), Tree),
            ((2, 0), Tree),
            ((2, 1), Open),
            ((2, 2), Open),
            ((2, 3), Tree),
            ((2, 4), Open),
            ((2, 5), Tree),
            ((2, 6), Open),
            ((2, 7), Open),
            ((2, 8), Tree),
            ((2, 9), Open),
            ((2, 10), Open),
            ((3, 0), Tree),
            ((3, 1), Open),
            ((3, 2), Open),
            ((3, 3), Open),
            ((3, 4), Open),
            ((3, 5), Open),
            ((3, 6), Tree),
            ((3, 7), Open),
            ((3, 8), Tree),
            ((3, 9), Open),
            ((3, 10), Open),
            ((4, 0), Open),
            ((4, 1), Tree),
            ((4, 2), Open),
            ((4, 3), Tree),
            ((4, 4), Open),
            ((4, 5), Tree),
            ((4, 6), Open),
            ((4, 7), Open),
            ((4, 8), Open),
            ((4, 9), Tree),
            ((4, 10), Tree),
            ((5, 0), Open),
            ((5, 1), Open),
            ((5, 2), Open),
            ((5, 3), Open),
            ((5, 4), Tree),
            ((5, 5), Tree),
            ((5, 6), Tree),
            ((5, 7), Open),
            ((5, 8), Open),
            ((5, 9), Tree),
            ((5, 10), Open),
            ((6, 0), Open),
            ((6, 1), Open),
            ((6, 2), Tree),
            ((6, 3), Open),
            ((6, 4), Tree),
            ((6, 5), Open),
            ((6, 6), Open),
            ((6, 7), Open),
            ((6, 8), Open),
            ((6, 9), Open),
            ((6, 10), Open),
            ((7, 0), Open),
            ((7, 1), Open),
            ((7, 2), Open),
            ((7, 3), Open),
            ((7, 4), Open),
            ((7, 5), Open),
            ((7, 6), Open),
            ((7, 7), Open),
            ((7, 8), Tree),
            ((7, 9), Open),
            ((7, 10), Open),
            ((8, 0), Open),
            ((8, 1), Tree),
            ((8, 2), Open),
            ((8, 3), Tree),
            ((8, 4), Open),
            ((8, 5), Open),
            ((8, 6), Open),
            ((8, 7), Open),
            ((8, 8), Open),
            ((8, 9), Open),
            ((8, 10), Tree),
            ((9, 0), Open),
            ((9, 1), Open),
            ((9, 2), Tree),
            ((9, 3), Open),
            ((9, 4), Tree),
            ((9, 5), Open),
            ((9, 6), Open),
            ((9, 7), Open),
            ((9, 8), Open),
            ((9, 9), Open),
            ((9, 10), Open),
            ((10, 0), Open),
            ((10, 1), Open),
            ((10, 2), Open),
            ((10, 3), Tree),
            ((10, 4), Open),
            ((10, 5), Open),
            ((10, 6), Tree),
            ((10, 7), Tree),
            ((10, 8), Open),
            ((10, 9), Tree),
            ((10, 10), Tree)
          ]
    }

spec :: Spec
spec = do
  it "parses sled maps" $ do
    parse exampleInput `shouldBe'` exampleSledMap

  it "calculates trees in the path" $ do
    treesPerSlope exampleSledMap Slope {right = 1, down = 1} `shouldBe` Right 2
    treesPerSlope exampleSledMap Slope {right = 3, down = 1} `shouldBe` Right 7
    treesPerSlope exampleSledMap Slope {right = 5, down = 1} `shouldBe` Right 3
    treesPerSlope exampleSledMap Slope {right = 7, down = 1} `shouldBe` Right 4
    treesPerSlope exampleSledMap Slope {right = 1, down = 2} `shouldBe` Right 2
