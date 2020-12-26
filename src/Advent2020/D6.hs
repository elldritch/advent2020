module Advent2020.D6 (run, part1, part2) where

import Advent2020.Internal.D6 (Group, parse)
import Data.Set (intersection, union)
import Relude
import Relude.Extra.Map

run :: (Group -> Set Char) -> Text -> Either Text Int
run runner contents = do
  groups <- parse contents
  return $ sum $ size . runner <$> groups

part1 :: Group -> Set Char
part1 = foldr union mempty

part2 :: Group -> Set Char
part2 = foldr intersection $ fromList ['a' .. 'z']
