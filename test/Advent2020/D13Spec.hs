module Advent2020.D13Spec (spec) where

import Advent2020.D13 (part2)
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

exampleSchedule2 :: Schedule
exampleSchedule2 =
  Schedule
    { earliestDeparture = error "should not be used",
      buses = [Just 17, Nothing, Just 13, Just 19]
    }

exampleSchedule3 :: Schedule
exampleSchedule3 =
  Schedule
    { earliestDeparture = error "should not be used",
      buses = [Just 67, Just 7, Just 59, Just 61]
    }

exampleSchedule4 :: Schedule
exampleSchedule4 =
  Schedule
    { earliestDeparture = error "should not be used",
      buses = [Just 67, Nothing, Just 7, Just 59, Just 61]
    }

exampleSchedule5 :: Schedule
exampleSchedule5 =
  Schedule
    { earliestDeparture = error "should not be used",
      buses = [Just 67, Just 7, Nothing, Just 59, Just 61]
    }

exampleSchedule6 :: Schedule
exampleSchedule6 =
  Schedule
    { earliestDeparture = error "should not be used",
      buses = [Just 1789, Just 37, Just 47, Just 1889]
    }

spec :: Spec
spec = do
  it "parses shuttle schedules" $ do
    parse exampleInput `shouldBe'` exampleSchedule

  it "finds the earliest bus" $ do
    let activeBuses = catMaybes $ buses exampleSchedule
    earliestBusAfter 939 (fromList activeBuses) `shouldBe` (59, 944)

  it "finds contest-winning bus timestamps" $ do
    part2 exampleSchedule `shouldBe'` 1068781
    part2 exampleSchedule2 `shouldBe'` 3417
    part2 exampleSchedule3 `shouldBe'` 754018
    part2 exampleSchedule4 `shouldBe'` 779210
    part2 exampleSchedule5 `shouldBe'` 1261476
    part2 exampleSchedule6 `shouldBe'` 1202161486
