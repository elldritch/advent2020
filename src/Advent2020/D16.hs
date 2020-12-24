module Advent2020.D16 (run, part1) where

import Relude
import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D16 (Range, Ticket, Rules, parse)

run :: ((Rules, Ticket, [Ticket]) -> Either Text Int) -> Text -> Either Text Int
run = simpleRun parse

part1 :: (Rules, Ticket, [Ticket]) -> Either Text Int
part1 (rules, _, tickets) = return $ sum $ foldr collectInvalidValues [] tickets
  where
    withinRange :: Range -> Int -> Bool
    withinRange (low, high) x = x >= low && x <= high

    canBeValid :: Int -> Bool
    canBeValid x = or $ (\ranges -> or $ (`withinRange` x) <$> ranges) <$> toList rules

    collectInvalidValues :: Ticket -> [Int] -> [Int]
    collectInvalidValues ticket prev = prev ++ foldr (\v acc -> acc ++ [v | not (canBeValid v)]) [] ticket
