module Advent2020.D12Spec (spec) where

import Advent2020.Internal.D12 (Action (..), Instruction (..), parse)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it)

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
  [ Instruction {action = MoveForward, value = 10},
    Instruction {action = MoveNorth, value = 3},
    Instruction {action = MoveForward, value = 7},
    Instruction {action = TurnRight, value = 90},
    Instruction {action = MoveForward, value = 11}
  ]

spec :: Spec
spec = do
  it "parses instructions" $ do
    parse exampleInput `shouldBe'` exampleInstructions
