module Advent2020.D17 (run, part1) where

import Advent2020.Internal (simpleRun, unsafeNonEmpty)
import Advent2020.Internal.D17 (Pocket (..), parse, step)
import Relude
import Relude.Extra.Map

run :: (Pocket -> Either Text output) -> Text -> Either Text output
run = simpleRun parse

part1 :: Pocket -> Either Text Int
part1 = return . size . activeCubes . head . unsafeNonEmpty . drop 6 . iterate step
