module Advent2020.D13 (run, part1) where

import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D13 (Schedule (..), earliestBusAfter, parse)
import Relude

run :: (Schedule -> Either Text Int) -> Text -> Either Text Int
run = simpleRun parse

part1 :: Schedule -> Either Text Int
part1 Schedule {..} = do
  let activeBuses = catMaybes buses
  bs <- maybeToRight "no buses in service" $ nonEmpty activeBuses
  let (busID, departureTime) = earliestBusAfter earliestDeparture bs
  return $ busID * (departureTime - earliestDeparture)
