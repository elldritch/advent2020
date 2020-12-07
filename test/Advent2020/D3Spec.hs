module Advent2020.D3Spec (spec) where

import Advent2020.Internal.D3 (MapSquare (..), SledMap (..), Slope (..), parse, treesPerSlope)
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

exampleSledMap :: SledMap
exampleSledMap =
  SledMap
    { rows =
        [ [Open, Open, Tree, Tree, Open, Open, Open, Open, Open, Open, Open],
          [Tree, Open, Open, Open, Tree, Open, Open, Open, Tree, Open, Open],
          [Open, Tree, Open, Open, Open, Open, Tree, Open, Open, Tree, Open],
          [Open, Open, Tree, Open, Tree, Open, Open, Open, Tree, Open, Tree],
          [Open, Tree, Open, Open, Open, Tree, Tree, Open, Open, Tree, Open],
          [Open, Open, Tree, Open, Tree, Tree, Open, Open, Open, Open, Open],
          [Open, Tree, Open, Tree, Open, Tree, Open, Open, Open, Open, Tree],
          [Open, Tree, Open, Open, Open, Open, Open, Open, Open, Open, Tree],
          [Tree, Open, Tree, Tree, Open, Open, Open, Tree, Open, Open, Open],
          [Tree, Open, Open, Open, Tree, Tree, Open, Open, Open, Open, Tree],
          [Open, Tree, Open, Open, Tree, Open, Open, Open, Tree, Open, Tree]
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
