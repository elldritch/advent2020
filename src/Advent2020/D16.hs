module Advent2020.D16 (run, part1, part2) where

import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D16 (FieldName (..), Rules, Ticket, chooseNamesUntilAmbiguous, invalidFields, parse)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import Relude
import Relude.Extra.Map

run :: ((Rules, Ticket, NonEmpty Ticket) -> Either Text a) -> Text -> Either Text a
run = simpleRun parse

part1 :: (Rules, Ticket, NonEmpty Ticket) -> Either Text Integer
part1 (rules, _, tickets) = return $ foldr sumInvalidValues 0 tickets
  where
    sumInvalidValues :: Ticket -> Integer -> Integer
    sumInvalidValues ticket acc = acc + sum (catMaybes $ (`lookup` ticket) <$> toList (invalidFields rules ticket))

-- NOTE: this solution does not work in general. In general, we need
-- backtracking to resolve potentially ambiguities. However, our particular
-- puzzle input is constructed so that we can eliminate options one at a time
-- and always arrive at a single, unambiguous solution.
part2 :: (Rules, Ticket, NonEmpty Ticket) -> Either Text Integer
part2 (rules, ticket, tickets) = return $ product $ catMaybes values
  where
    validTickets = NonEmpty.filter (null . invalidFields rules) tickets
    names = chooseNamesUntilAmbiguous rules (ticket :| validTickets)
    departureFields = Map.filter (Text.isPrefixOf "departure" . unFieldName) names
    values = (`lookup` ticket) <$> keys departureFields
