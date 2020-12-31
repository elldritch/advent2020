module Advent2020.D11Spec (spec) where

import Advent2020.Internal (fixed)
import Advent2020.Internal.D11 (Grid (..), Position (..), adjacent, firstVisibleSeat, parse, step)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ]

exampleGrid :: Grid Position
exampleGrid =
  Grid
    { height = 10,
      width = 10,
      gridMap =
        fromList
          [ ((0, 0), Empty),
            ((0, 1), Empty),
            ((0, 2), Empty),
            ((0, 3), Empty),
            ((0, 4), Empty),
            ((0, 5), Empty),
            ((0, 6), Floor),
            ((0, 7), Empty),
            ((0, 8), Empty),
            ((0, 9), Empty),
            ((1, 0), Floor),
            ((1, 1), Empty),
            ((1, 2), Floor),
            ((1, 3), Empty),
            ((1, 4), Floor),
            ((1, 5), Floor),
            ((1, 6), Floor),
            ((1, 7), Empty),
            ((1, 8), Floor),
            ((1, 9), Floor),
            ((2, 0), Empty),
            ((2, 1), Empty),
            ((2, 2), Empty),
            ((2, 3), Empty),
            ((2, 4), Empty),
            ((2, 5), Empty),
            ((2, 6), Empty),
            ((2, 7), Empty),
            ((2, 8), Empty),
            ((2, 9), Empty),
            ((3, 0), Empty),
            ((3, 1), Empty),
            ((3, 2), Floor),
            ((3, 3), Empty),
            ((3, 4), Empty),
            ((3, 5), Empty),
            ((3, 6), Floor),
            ((3, 7), Empty),
            ((3, 8), Empty),
            ((3, 9), Empty),
            ((4, 0), Floor),
            ((4, 1), Empty),
            ((4, 2), Empty),
            ((4, 3), Floor),
            ((4, 4), Floor),
            ((4, 5), Empty),
            ((4, 6), Empty),
            ((4, 7), Empty),
            ((4, 8), Empty),
            ((4, 9), Empty),
            ((5, 0), Empty),
            ((5, 1), Empty),
            ((5, 2), Floor),
            ((5, 3), Empty),
            ((5, 4), Empty),
            ((5, 5), Empty),
            ((5, 6), Floor),
            ((5, 7), Empty),
            ((5, 8), Empty),
            ((5, 9), Empty),
            ((6, 0), Empty),
            ((6, 1), Empty),
            ((6, 2), Floor),
            ((6, 3), Empty),
            ((6, 4), Empty),
            ((6, 5), Empty),
            ((6, 6), Floor),
            ((6, 7), Empty),
            ((6, 8), Empty),
            ((6, 9), Empty),
            ((7, 0), Floor),
            ((7, 1), Floor),
            ((7, 2), Empty),
            ((7, 3), Floor),
            ((7, 4), Floor),
            ((7, 5), Floor),
            ((7, 6), Floor),
            ((7, 7), Empty),
            ((7, 8), Empty),
            ((7, 9), Floor),
            ((8, 0), Empty),
            ((8, 1), Empty),
            ((8, 2), Floor),
            ((8, 3), Empty),
            ((8, 4), Empty),
            ((8, 5), Empty),
            ((8, 6), Floor),
            ((8, 7), Empty),
            ((8, 8), Floor),
            ((8, 9), Empty),
            ((9, 0), Empty),
            ((9, 1), Empty),
            ((9, 2), Floor),
            ((9, 3), Empty),
            ((9, 4), Empty),
            ((9, 5), Empty),
            ((9, 6), Floor),
            ((9, 7), Empty),
            ((9, 8), Empty),
            ((9, 9), Empty)
          ]
    }

exampleInput2 :: Text
exampleInput2 =
  unlines
    [ ".......#.",
      "...#.....",
      ".#.......",
      ".........",
      "..#L....#",
      "....#....",
      ".........",
      "#........",
      "...#....."
    ]

exampleInput3 :: Text
exampleInput3 =
  unlines
    [ ".............",
      ".L.L.#.#.#.#.",
      "............."
    ]

exampleInput4 :: Text
exampleInput4 =
  unlines
    [ ".##.##.",
      "#.#.#.#",
      "##...##",
      "...L...",
      "##...##",
      "#.#.#.#",
      ".##.##."
    ]

visible :: Text -> (Int, Int) -> Either Text [Position]
visible input seat = parse input <&> (`firstVisibleSeat` seat)

spec :: Spec
spec = do
  it "parses seat layouts" $ do
    parse exampleInput `shouldBe'` exampleGrid

  it "finds the fixed point of layout steps" $ do
    length (filter (== Occupied) $ toList $ gridMap $ fixed (step adjacent 4) exampleGrid) `shouldBe` 37

  it "computes visible seats" $ do
    visible exampleInput2 (3, 4) `shouldBe'` replicate 8 Occupied
    visible exampleInput3 (1, 1) `shouldBe'` [Empty]
    visible exampleInput4 (3, 3) `shouldBe'` []

  it "finds fixed point layouts for people who care about visible seats" $ do
    length (filter (== Occupied) $ toList $ gridMap $ fixed (step firstVisibleSeat 5) exampleGrid) `shouldBe` 26
