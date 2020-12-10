module Advent2020.D6 (run, part1, part2) where

import Advent2020.Internal.D6 (Group, parse)
import Data.Set (intersection, size, union)
import Relude

run :: Text -> (Group -> Set Char) -> Either Text Int
run contents runner = do
  groups <- parse contents
  return $ sum $ size <$> fmap runner groups

part1 :: Group -> Set Char
part1 = foldr union mempty

part2 :: Group -> Set Char
part2 = foldr intersection $ fromList ['a' .. 'z']
