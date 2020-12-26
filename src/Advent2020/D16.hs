module Advent2020.D16 (run, part1, part2) where

import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D16 (FieldID (..), FieldName (..), Rules, Ticket, invalidFields, parse, possibleFields)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (lookup)
import qualified Data.Map as Map
import Data.Set (size)
import Relude

run :: ((Rules, Ticket, NonEmpty Ticket) -> Either Text a) -> Text -> Either Text a
run = simpleRun parse

part1 :: (Rules, Ticket, NonEmpty Ticket) -> Either Text Int
part1 (rules, _, tickets) = return $ foldr sumInvalidValues 0 tickets
  where
    sumInvalidValues :: Ticket -> Int -> Int
    sumInvalidValues ticket acc = acc + sum (catMaybes $ (`lookup` ticket) <$> toList (invalidFields rules ticket))

-- NOTE: this solution does not work in general. In general, we need
-- backtracking to resolve potentially ambiguities. However, our particular
-- puzzle input is constructed so that we can eliminate options one at a time
-- and always arrive at a single, unambiguous solution.
part2 :: (Rules, Ticket, NonEmpty Ticket) -> Either Text Text
part2 (rules, ticket, tickets) = return $ format $ possibleFields rules (ticket :| validTickets)
  where
    validTickets = NonEmpty.filter (null . invalidFields rules) tickets

    format :: Map FieldID (Set FieldName) -> Text
    format m = "\n\n" <> foldr (\(FieldID k, v) acc -> show k <> " (" <> show (size v) <> ")" <> ": " <> format' v <> "\n" <> acc) "" (sortWith (size . snd) $ Map.toList m)

    format' :: Set FieldName -> Text
    format' s = foldr (\(FieldName n) acc -> show n <> " " <> acc) "" $ sort $ toList s
