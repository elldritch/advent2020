module Advent2020.D14Spec (spec) where

import Advent2020.Internal.D14 (BitMask (..), DecodeMask, Instruction (..), Machine (..), MaskBit (..), Program, execute, parse, step, step')
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

exampleBitMaskProgram :: Program
exampleBitMaskProgram =
  [ SetMask [X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, One, X, X, X, X, Zero, X],
    SetMemory {address = 8, value = 11},
    SetMemory {address = 7, value = 101},
    SetMemory {address = 8, value = 0}
  ]

exampleBitMaskMachine :: Machine BitMask
exampleBitMaskMachine =
  Machine
    { memory =
        fromList
          [ (7, 101),
            (8, 64)
          ],
      mask = BitMask {orMask = 64, andMask = 68719476733}
    }

exampleDecodeMaskProgram :: Program
exampleDecodeMaskProgram =
  fromRight (error "parser is broken") $
    parse $
      unlines
        [ "mask = 000000000000000000000000000000X1001X",
          "mem[42] = 100",
          "mask = 00000000000000000000000000000000X0XX",
          "mem[26] = 1"
        ]

exampleDecodeMaskMachine :: Machine DecodeMask
exampleDecodeMaskMachine =
  Machine
    { memory =
        fromList
          [ (58, 100),
            (59, 100),
            (16, 1),
            (17, 1),
            (18, 1),
            (19, 1),
            (24, 1),
            (25, 1),
            (26, 1),
            (27, 1)
          ],
      mask = [Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, X, Zero, X, X]
    }

spec :: Spec
spec = do
  it "parses programs" $ do
    parse exampleInput `shouldBe'` exampleBitMaskProgram

  it "runs programs" $ do
    execute step exampleBitMaskProgram `shouldBe` exampleBitMaskMachine

  it "runs memory-decoded programs" $ do
    execute step' exampleDecodeMaskProgram `shouldBe` exampleDecodeMaskMachine
