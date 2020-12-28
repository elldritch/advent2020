module Advent2020.D19 (run, part1, part2) where

import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D19 (Message, Rules, match, parse, updateRules)
import Relude

run :: ((Rules, [Message]) -> Either Text Int) -> Text -> Either Text Int
run = simpleRun parse

part1 :: (Rules, [Message]) -> Either Text Int
part1 (rules, messages) = return $ length $ filter (match rules) messages

part2 :: (Rules, [Message]) -> Either Text Int
part2 = part1 . first updateRules
