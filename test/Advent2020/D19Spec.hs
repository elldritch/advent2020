{-# LANGUAGE QuasiQuotes #-}

module Advent2020.D19Spec (spec) where

import Advent2020.Internal.D19 (Message, Rule (..), Rules, match, parse, parseRules)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

exampleInput :: Text
exampleInput =
  [r|0: 1 2
1: "a"
2: 1 3 | 3 1
3: "b"
|]

exampleRules :: Rules
exampleRules =
  fromList
    [ (0, SubRules [[1, 2]]),
      (1, Literal 'a'),
      (2, SubRules [[1, 3], [3, 1]]),
      (3, Literal 'b')
    ]

exampleInput2 :: Text
exampleInput2 =
  [r|0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb
|]

exampleRules2 :: Rules
exampleRules2 =
  fromList
    [ (0, SubRules [[4, 1, 5]]),
      (1, SubRules [[2, 3], [3, 2]]),
      (2, SubRules [[4, 4], [5, 5]]),
      (3, SubRules [[4, 5], [5, 4]]),
      (4, Literal 'a'),
      (5, Literal 'b')
    ]

exampleMessages2 :: [Message]
exampleMessages2 = ["ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb"]

spec :: Spec
spec = do
  it "parses message rules" $ do
    parseRules exampleInput `shouldBe'` exampleRules

  it "parses puzzle inputs" $ do
    parse exampleInput2 `shouldBe'` (exampleRules2, exampleMessages2)

  it "matches messages with rules" $ do
    match exampleRules2 <$> exampleMessages2 `shouldBe` [True, False, True, False, False]
