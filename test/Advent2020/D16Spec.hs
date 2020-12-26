module Advent2020.D16Spec (spec) where

import Advent2020.Internal.D16 (FieldID (..), FieldName (..), Rules, Ticket, chooseNamesUntilAmbiguous, invalidFields, parse, possibleFieldNames)
import Advent2020.Spec.Internal (shouldBe')
import qualified Data.List.NonEmpty as NonEmpty
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

ticket :: [(Int, Integer)] -> Ticket
ticket fields = fromList $ first FieldID <$> fields

tickets' :: NonEmpty [(Int, Integer)] -> NonEmpty Ticket
tickets' ts = ticket <$> ts

exampleTicket :: Ticket
exampleTicket = ticket [(0, 7), (1, 1), (2, 14)]

exampleTicket2 :: Ticket
exampleTicket2 = ticket [(0, 11), (1, 12), (2, 13)]

exampleOtherTickets :: NonEmpty Ticket
exampleOtherTickets =
  tickets' $
    [(0, 7), (1, 3), (2, 47)]
      :| [ [(0, 40), (1, 4), (2, 50)],
           [(0, 55), (1, 2), (2, 20)],
           [(0, 38), (1, 6), (2, 12)]
         ]

exampleOtherTickets2 :: NonEmpty Ticket
exampleOtherTickets2 =
  tickets' $
    [(0, 3), (1, 9), (2, 18)]
      :| [ [(0, 15), (1, 1), (2, 5)],
           [(0, 5), (1, 14), (2, 9)]
         ]

exampleInvalidTickets :: [(Ticket, Set FieldID)]
exampleInvalidTickets =
  [ (ticket [(0, 40), (1, 4), (2, 50)], one $ FieldID 1),
    (ticket [(0, 55), (1, 2), (2, 20)], one $ FieldID 0),
    (ticket [(0, 38), (1, 6), (2, 12)], one $ FieldID 2)
  ]

examplePossibleFields :: Map FieldID (Set FieldName)
examplePossibleFields =
  fromList
    [ (FieldID 0, fromList ["row"]),
      (FieldID 1, fromList ["class", "row"]),
      (FieldID 2, fromList ["class", "row", "seat"])
    ]

exampleFieldChoices :: Map FieldID FieldName
exampleFieldChoices =
  fromList
    [ (FieldID 0, "row"),
      (FieldID 1, "class"),
      (FieldID 2, "seat")
    ]

invalidTickets :: Rules -> NonEmpty Ticket -> [(Ticket, Set FieldID)]
invalidTickets rules tickets =
  NonEmpty.filter (not . null . snd) $ NonEmpty.zip tickets $ invalidFields rules <$> tickets

spec :: Spec
spec = do
  it "parses ticket notes" $ do
    parse exampleInput `shouldBe'` (exampleRules, exampleTicket, exampleOtherTickets)
    parse exampleInput2 `shouldBe'` (exampleRules2, exampleTicket2, exampleOtherTickets2)

  it "detects fields that can never possibly be valid" $ do
    invalidTickets exampleRules exampleOtherTickets `shouldBe` exampleInvalidTickets
    invalidTickets exampleRules2 exampleOtherTickets2 `shouldBe` []

  it "determines possible field names" $ do
    possibleFieldNames exampleRules2 exampleOtherTickets2 `shouldBe` examplePossibleFields

  it "makes non-ambiguous field name choices" $ do
    chooseNamesUntilAmbiguous exampleRules2 exampleOtherTickets2 `shouldBe` exampleFieldChoices
