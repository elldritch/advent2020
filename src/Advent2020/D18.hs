module Advent2020.D18 (run, part1) where

import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D18 (Expr, eval, parse)
import Relude

run :: ([Expr] -> Either Text Integer) -> Text -> Either Text Integer
run = simpleRun parse

part1 :: [Expr] -> Either Text Integer
part1 = return . sum . fmap eval
