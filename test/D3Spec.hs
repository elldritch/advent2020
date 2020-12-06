module D3Spec (spec) where

import Advent2020.D3 (part2, MapSquare (..), SledMap (..), Slope (..), parse, treesPerSlope)
import Relude
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  let exampleInput =
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
  let exampleSledMap =
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

  describe "part 1" $ do
    it "parses sled maps" $ do
      parse exampleInput `shouldBe` Right exampleSledMap
    it "calculates trees in the path" $ do
      treesPerSlope exampleSledMap Slope {right = 1, down = 1} `shouldBe` Right 2
      treesPerSlope exampleSledMap Slope {right = 3, down = 1} `shouldBe` Right 7
      treesPerSlope exampleSledMap Slope {right = 5, down = 1} `shouldBe` Right 3
      treesPerSlope exampleSledMap Slope {right = 7, down = 1} `shouldBe` Right 4
      treesPerSlope exampleSledMap Slope {right = 1, down = 2} `shouldBe` Right 2

  describe "part 2" $ do
    it "calculates the product of encountered trees" $ do
      part2 exampleSledMap `shouldBe` Right 336
