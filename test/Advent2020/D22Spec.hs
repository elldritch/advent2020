{-# LANGUAGE QuasiQuotes #-}

module Advent2020.D22Spec (spec) where

import Advent2020.Internal.D22 (Deck, Player (..), parse, play, play', score)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

exampleInput :: Text
exampleInput =
  [r|Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
|]

exampleDeck1 :: Deck
exampleDeck1 = 9 :| [2, 6, 3, 1]

exampleDeck2 :: Deck
exampleDeck2 = 5 :| [8, 4, 7, 10]

exampleDeck3 :: Deck
exampleDeck3 = 3 :| [2, 10, 6, 8, 5, 9, 4, 7, 1]

spec :: Spec
spec = do
  it "parses Combat decks" $ do
    parse exampleInput `shouldBe'` (exampleDeck1, exampleDeck2)

  it "plays Combat" $ do
    play (exampleDeck1, exampleDeck2) `shouldBe` (Two, 3 :| [2, 10, 6, 8, 5, 9, 4, 7, 1])

  it "scores Combat decks" $ do
    score exampleDeck3 `shouldBe` 306

  it "plays Recursive Combat" $ do
    play' (exampleDeck1, exampleDeck2) `shouldBe` (Two, 7 :| [5, 6, 2, 4, 1, 10, 8, 9, 3])
