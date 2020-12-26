module Advent2020.D17Spec (spec) where

import Advent2020.Internal (unsafeNonEmpty)
import Advent2020.Internal.D17 (Pocket (..), parse, step)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Relude.Extra.Map
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ ".#.",
      "..#",
      "###"
    ]

examplePocket :: Pocket
examplePocket = Pocket $ fromList $ (\(x, y) -> (x, y, 0)) <$> actives
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
  it "parses pocket dimension slices" $ do
    parse exampleInput `shouldBe'` examplePocket

  it "simulates pocket dimension steps" $ do
    let result = head $ unsafeNonEmpty $ drop 6 $ iterate step examplePocket
    size (activeCubes result) `shouldBe` 112
