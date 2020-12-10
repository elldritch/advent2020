module Advent2020.D8Spec (spec) where

import Advent2020.D8 (part1)
import Advent2020.Internal.D8 (Instruction (..), Operation (..), Program, parse)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it)

exampleInput :: Text
exampleInput =
  unlines
    [ "nop +0",
      "acc +1",
      "jmp +4",
      "acc +3",
      "jmp -3",
      "acc -99",
      "acc +1",
      "jmp -4",
      "acc +6"
    ]

exampleProgram :: Program
exampleProgram =
  [ Instruction {operation = NoOp, argument = 0},
    Instruction {operation = Accumulate, argument = 1},
    Instruction {operation = Jump, argument = 4},
    Instruction {operation = Accumulate, argument = 3},
    Instruction {operation = Jump, argument = -3},
    Instruction {operation = Accumulate, argument = -99},
    Instruction {operation = Accumulate, argument = 1},
    Instruction {operation = Jump, argument = -4},
    Instruction {operation = Accumulate, argument = 6}
  ]

spec :: Spec
spec = do
  it "parses programs" $ do
    parse exampleInput `shouldBe'` exampleProgram

  it "finds program loops" $ do
    part1 exampleInput `shouldBe'` 5
