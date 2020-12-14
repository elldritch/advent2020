module Advent2020.D13Spec (spec) where

import Advent2020.Internal.D13 (Schedule (..), earliestBusAfter, parse)
import Advent2020.Spec.Internal (shouldBe')
import Relude
import Test.Hspec (Spec, it, shouldBe)

exampleInput :: Text
exampleInput =
  unlines
    [ "939",
      "7,13,x,x,59,x,31,19"
    ]

exampleSchedule :: Schedule
exampleSchedule =
  Schedule
    { earliestDeparture = 939,
      buses =
        [ Just 7,
          Just 13,
          Nothing,
          Nothing,
          Just 59,
          Nothing,
          Just 31,
          Just 19
        ]
    }

spec :: Spec
spec = do
  it "parses shuttle schedules" $ do
    parse exampleInput `shouldBe'` exampleSchedule

  it "finds the earliest bus" $ do
    let activeBuses = catMaybes $ buses exampleSchedule
    earliestBusAfter 939 (fromList activeBuses) `shouldBe` (59, 944)
