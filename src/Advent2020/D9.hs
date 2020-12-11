module Advent2020.D9 (part1) where

import Advent2020.Internal (parseNumbers)
import Advent2020.Internal.D9 (findWeakNumber)
import Relude

part1 :: Text -> Either Text Int
part1 contents = do
  ns <- parseNumbers contents
  findWeakNumber 25 ns
