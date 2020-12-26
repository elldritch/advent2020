module Advent2020.D16 (run, part1) where

import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D16 (FieldID (..), Rules, Ticket, invalidTickets, parse)
import Data.Map (lookup)
import Relude
import qualified Relude.Unsafe as Unsafe

run :: ((Rules, Ticket, [Ticket]) -> Either Text Int) -> Text -> Either Text Int
run = simpleRun parse

part1 :: (Rules, Ticket, [Ticket]) -> Either Text Int
part1 (rules, _, tickets) = return $ foldr sumInvalidValues 0 $ invalidTickets rules tickets
  where
    sumInvalidValues :: (Ticket, Set FieldID) -> Int -> Int
    sumInvalidValues (ticket, keys) acc = acc + foldr (\k acc' -> acc' + Unsafe.fromJust (lookup k ticket)) 0 keys
