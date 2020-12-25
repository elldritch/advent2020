module Advent2020.D16Spec (spec) where

import Advent2020.Internal.D16 (Rules, Ticket, invalidTickets, parse)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ "class: 1-3 or 5-7",
      "row: 6-11 or 33-44",
      "seat: 13-40 or 45-50",
      "",
      "your ticket:",
      "7,1,14",
      "",
      "nearby tickets:",
      "7,3,47",
      "40,4,50",
      "55,2,20",
      "38,6,12"
    ]

exampleRules :: Rules
exampleRules =
  fromList
    [ ("class", [(1, 3), (5, 7)]),
      ("row", [(6, 11), (33, 44)]),
      ("seat", [(13, 40), (45, 50)])
    ]

exampleTicket :: Ticket
exampleTicket =
  fromList [("0", 7), ("1", 1), ("2", 14)]

exampleOtherTickets :: [Ticket]
exampleOtherTickets =
  [ fromList [("0", 7), ("1", 3), ("2", 47)],
    fromList [("0", 40), ("1", 4), ("2", 50)],
    fromList [("0", 55), ("1", 2), ("2", 20)],
    fromList [("0", 38), ("1", 6), ("2", 12)]
  ]

exampleInvalidTickets :: [(Ticket, Set Text)]
exampleInvalidTickets =
  [ (fromList [("0", 40), ("1", 4), ("2", 50)], one "1"),
    (fromList [("0", 55), ("1", 2), ("2", 20)], one "0"),
    (fromList [("0", 38), ("1", 6), ("2", 12)], one "2")
  ]

spec :: Spec
spec = do
  it "parses ticket notes" $ do
    parse exampleInput `shouldBe'` (exampleRules, exampleTicket, exampleOtherTickets)

  it "detects fields that can never possibly be valid" $ do
    invalidTickets exampleRules exampleOtherTickets `shouldBe` exampleInvalidTickets
