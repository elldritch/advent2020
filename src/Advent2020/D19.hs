module Advent2020.D19 (run, part1) where

import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D19 (Message, Rules, match, parse)
import Relude

run :: ((Rules, [Message]) -> Either Text Int) -> Text -> Either Text Int
run = simpleRun parse

part1 :: (Rules, [Message]) -> Either Text Int
part1 (rules, messages) = return $ length $ filter (match rules) messages
