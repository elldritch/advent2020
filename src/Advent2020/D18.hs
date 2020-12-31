module Advent2020.D18 (run, part1, part2) where

import Advent2020.Internal.D18 (Expr, eval, parse, parse')
import Relude

run :: (Text -> Either Text [Expr]) -> Text -> Either Text Integer
run parser contents = parser contents <&> sum . fmap eval

part1 :: (Text -> Either Text [Expr])
part1 = parse

part2 :: (Text -> Either Text [Expr])
part2 = parse'
