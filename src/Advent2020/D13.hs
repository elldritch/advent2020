module Advent2020.D13 (run, part1, part2) where

import Advent2020.Internal (simpleRun, tracePrefix)
import Advent2020.Internal.D13 (Schedule (..), chinese', earliestBusAfter, parse)
import Relude
import Relude.Extra.Bifunctor (bimapBoth)
import qualified Relude.Unsafe as Unsafe

run :: (Schedule -> Either Text Int) -> Text -> Either Text Int
run = simpleRun parse

part1 :: Schedule -> Either Text Int
part1 Schedule {..} = do
  let activeBuses = catMaybes buses
  bs <- maybeToRight "no buses in service" $ nonEmpty activeBuses
  let (busID, departureTime) = earliestBusAfter earliestDeparture bs
  return $ busID * (departureTime - earliestDeparture)

-- This problem reduces to the Chinese Remainder Theorem. See https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Existence_(constructive_proof).
part2 :: Schedule -> Either Text Int
part2 Schedule {..} = do
  offsets' <- maybeToRight "no buses in service" $ nonEmpty offsets
  (n, m) <- maybeToRight "no such timestamp (buses are periodic)" $ chinese' $ bimapBoth toInteger <$> tracePrefix "offsets'" offsets'
  return $ fromInteger $ if n >= 0 then n else m + n
  where
    offsets :: [(Int, Int)]
    offsets = second Unsafe.fromJust <$> filter (isJust . snd) (zip [0, -1 ..] buses)
