module Advent2020.D14Spec (spec) where

import Advent2020.Internal.D14 (Instruction (..), Machine (..), Mask (..), Program, parse, execute)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
    ]

exampleProgram :: Program
exampleProgram =
  [ SetMask
      ( Mask
          { orMask = 0b000000000000000000000000000001000000,
            andMask = 0b111111111111111111111111111111111101
          }
      ),
    SetMemory {address = 8, value = 11},
    SetMemory {address = 7, value = 101},
    SetMemory {address = 8, value = 0}
  ]

spec :: Spec
spec = do
  it "parses programs" $ do
    parse exampleInput `shouldBe'` exampleProgram

  it "runs programs" $ do
    execute exampleProgram `shouldBe` Machine {memory = fromList [(7,101),(8,64)], mask = Mask {orMask = 64, andMask = 68719476733}}
