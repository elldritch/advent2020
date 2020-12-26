module Advent2020.D16Spec (spec) where

import Advent2020.Internal (unsafeNonEmpty)
import Advent2020.Internal.D16 (FieldID (..), FieldName (..), Rules, Ticket, invalidTickets, parse, possibleFields)
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

exampleInput2 :: Text
exampleInput2 =
  unlines
    [ "class: 0-1 or 4-19",
      "row: 0-5 or 8-19",
      "seat: 0-13 or 16-19",
      "",
      "your ticket:",
      "11,12,13",
      "",
      "nearby tickets:",
      "3,9,18",
      "15,1,5",
      "5,14,9"
    ]

exampleRules :: Rules
exampleRules =
  fromList
    [ ("class", [(1, 3), (5, 7)]),
      ("row", [(6, 11), (33, 44)]),
      ("seat", [(13, 40), (45, 50)])
    ]

exampleRules2 :: Rules
exampleRules2 =
  fromList
    [ ("class", [(0, 1), (4, 19)]),
      ("row", [(0, 5), (8, 19)]),
      ("seat", [(0, 13), (16, 19)])
    ]

exampleTicket :: Ticket
exampleTicket =
  fromList [("0", 7), ("1", 1), ("2", 14)]

exampleTicket2 :: Ticket
exampleTicket2 =
  fromList [("0", 11), ("1", 12), ("2", 13)]

exampleOtherTickets :: [Ticket]
exampleOtherTickets =
  [ fromList [("0", 7), ("1", 3), ("2", 47)],
    fromList [("0", 40), ("1", 4), ("2", 50)],
    fromList [("0", 55), ("1", 2), ("2", 20)],
    fromList [("0", 38), ("1", 6), ("2", 12)]
  ]

exampleOtherTickets2 :: [Ticket]
exampleOtherTickets2 =
  [ fromList [("0", 3), ("1", 9), ("2", 18)],
    fromList [("0", 15), ("1", 1), ("2", 5)],
    fromList [("0", 5), ("1", 14), ("2", 9)]
  ]

exampleInvalidTickets :: [(Ticket, Set FieldID)]
exampleInvalidTickets =
  [ (fromList [("0", 40), ("1", 4), ("2", 50)], one "1"),
    (fromList [("0", 55), ("1", 2), ("2", 20)], one "0"),
    (fromList [("0", 38), ("1", 6), ("2", 12)], one "2")
  ]

examplePossibleFields :: Map FieldID (Set FieldName)
examplePossibleFields =
  fromList
    [ ( FieldID {unFieldID = "0"},
        fromList
          [ FieldName {unFieldName = "row"}
          ]
      ),
      ( FieldID {unFieldID = "1"},
        fromList
          [ FieldName {unFieldName = "class"},
            FieldName {unFieldName = "row"}
          ]
      ),
      ( FieldID {unFieldID = "2"},
        fromList
          [ FieldName {unFieldName = "class"},
            FieldName {unFieldName = "row"},
            FieldName {unFieldName = "seat"}
          ]
      )
    ]

spec :: Spec
spec = do
  it "parses ticket notes" $ do
    parse exampleInput `shouldBe'` (exampleRules, exampleTicket, exampleOtherTickets)
    parse exampleInput2 `shouldBe'` (exampleRules2, exampleTicket2, exampleOtherTickets2)

  it "detects fields that can never possibly be valid" $ do
    invalidTickets exampleRules exampleOtherTickets `shouldBe` exampleInvalidTickets
    invalidTickets exampleRules2 exampleOtherTickets2 `shouldBe` []

  it "determines possible field names" $ do
    possibleFields exampleRules2 (unsafeNonEmpty exampleOtherTickets2) `shouldBe` examplePossibleFields
