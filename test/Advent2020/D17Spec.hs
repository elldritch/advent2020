module Advent2020.D17Spec (spec) where

import Advent2020.Internal.D17 (Cube, Hypercube, Pocket (..), numCubes, parse, parse', stepN, stepN')
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, describe, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ ".#.",
      "..#",
      "###"
    ]

examplePocket :: Pocket Cube
examplePocket = Pocket $ fromList $ (\(x, y) -> (x, y, 0)) <$> actives
  where
    actives =
      [ (1, 0),
        (2, 1),
        (0, 2),
        (1, 2),
        (2, 2)
      ]

exampleHyperpocket :: Pocket Hypercube
exampleHyperpocket = Pocket $ fromList $ (\(x, y) -> (x, y, 0, 0)) <$> actives
  where
    actives =
      [ (1, 0),
        (2, 1),
        (0, 2),
        (1, 2),
        (2, 2)
      ]

spec :: Spec
spec = do
  describe "3-space pocket dimensions" $ do
    it "parses pocket dimension slices" $ do
      parse exampleInput `shouldBe'` examplePocket

    it "simulates pocket dimension steps" $ do
      numCubes (stepN 6 examplePocket) `shouldBe` 112

  describe "4-space pocket dimensions" $ do
    it "parses pocket dimension slices" $ do
      parse' exampleInput `shouldBe'` exampleHyperpocket

    it "simulates pocket dimension steps" $ do
      numCubes (stepN' 6 exampleHyperpocket) `shouldBe` 848
