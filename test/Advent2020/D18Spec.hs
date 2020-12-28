module Advent2020.D18Spec (spec) where

import Advent2020.Internal.D18 (Expr (..), eval, parse, parse')
import Advent2020.Spec.Internal (fromRight', shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ "1 + 2 * 3 + 4 * 5 + 6",
      "1 + (2 * 3) + (4 * (5 + 6))"
    ]

exampleExprs :: [Expr]
exampleExprs =
  [ Add (Multiply (Add (Multiply (Add (Operand 1) (Operand 2)) (Operand 3)) (Operand 4)) (Operand 5)) (Operand 6),
    Add (Add (Operand 1) (Multiply (Operand 2) (Operand 3))) (Multiply (Operand 4) (Add (Operand 5) (Operand 6)))
  ]

exampleInput2 :: Text
exampleInput2 =
  unlines
    [ "2 * 3 + (4 * 5)",
      "5 + (8 * 3 + 9 + 3 * 4 * 3)",
      "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
      "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
    ]

spec :: Spec
spec = do
  it "parses weird math expressions" $ do
    parse exampleInput `shouldBe'` exampleExprs

  it "evaluates weird math expressions" $ do
    eval <$> exampleExprs `shouldBe` [71, 51]
    exprs <- fromRight' $ parse exampleInput2
    eval <$> exprs `shouldBe` [26, 437, 12240, 13632]

  it "evaluates advanced weird math expressions" $ do
    exprs <- fromRight' $ parse' $ exampleInput <> exampleInput2
    eval <$> exprs `shouldBe` [231, 51, 46, 1445, 669060, 23340]
