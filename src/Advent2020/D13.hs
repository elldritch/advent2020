module Advent2020.D13 (run, part1, part2) where

import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D13 (Schedule (..), chinese', earliestBusAfter, parse)
import Relude
import qualified Relude.Unsafe as Unsafe

run :: (Schedule -> Either Text t) -> Text -> Either Text t
run = simpleRun parse

part1 :: Schedule -> Either Text Int
part1 Schedule {..} = do
  let activeBuses = catMaybes buses
  bs <- maybeToRight "no buses in service" $ nonEmpty activeBuses
  let (busID, departureTime) = earliestBusAfter earliestDeparture bs
  return $ busID * (departureTime - earliestDeparture)

-- This problem reduces to the Chinese Remainder Theorem. See
-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Existence_(constructive_proof).
--
-- NOTE: This only works because the bus IDs in the input happen to be pairwise
-- coprime. In general, this does not work for arbitrary bus schedules. However,
-- this problem is constructed such that a solution exists (which I suppose they
-- imply by asking for a solution).
part2 :: Schedule -> Either Text Integer
part2 Schedule {..} = do
  offsets' <- maybeToRight "no buses in service" $ nonEmpty offsets
  (n, m) <- maybeToRight "no such timestamp (buses are periodic)" $ chinese' offsets'
  return $ if n >= 0 then n else m + n
  where
    offsets :: [(Integer, Integer)]
    offsets = second (toInteger . Unsafe.fromJust) <$> filter (isJust . snd) (zip [0, -1 ..] buses)
