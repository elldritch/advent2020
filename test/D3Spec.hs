module D3Spec (spec) where

import Advent2020.D3 (part1, run, MapSquare (..), SledMap (..), parse)
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
      run exampleInput part1 `shouldBe` Right 7
