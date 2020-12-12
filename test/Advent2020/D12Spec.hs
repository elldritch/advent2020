module Advent2020.D12Spec (spec) where

import Advent2020.Internal.D12 (Action (..), Direction (..), Instruction (..), Navigation (..), Orientation (..), Ship (..), initial, initial', parse, step, step')
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ "F10",
      "N3",
      "F7",
      "R90",
      "F11"
    ]

exampleInstructions :: [Instruction]
exampleInstructions =
  [ Instruction {action = Forward, value = 10},
    Instruction {action = Move North, value = 3},
    Instruction {action = Forward, value = 7},
    Instruction {action = Turn DRight, value = 90},
    Instruction {action = Forward, value = 11}
  ]

spec :: Spec
spec = do
  it "parses instructions" $ do
    parse exampleInput `shouldBe'` exampleInstructions

  it "follows ship instructions" $ do
    foldl' step initial exampleInstructions `shouldBe` Ship {orientation = South, position = (17, -8)}

  it "follows waypoint navigation" $ do
    foldl' step' initial' exampleInstructions `shouldBe` Navigation {ship = (214, -72), waypoint = (4, -10)}
