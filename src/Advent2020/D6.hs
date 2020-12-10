module Advent2020.D6 (part1) where

import Advent2020.Internal.D6 (parse)
import Data.Set (size, union)
import Relude

part1 :: Text -> Either Text Int
part1 contents = do
  groups <- parse contents
  return $ sum $ size <$> fmap (foldr union mempty) groups
