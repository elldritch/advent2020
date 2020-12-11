module Advent2020.Internal.D9 (findWeakNumber) where

import Advent2020.Internal (pairs, windows)
import Relude

findWeakNumber :: Int -> [Int] -> Either Text Int
findWeakNumber preambleSize ns = do
  (i, _) <- maybeToRight "could not find weak number" $ find (\(n, w) -> not $ isSumOfPairs n w) $ zip (drop preambleSize ns) $ windows preambleSize ns
  return i
  where
    isSumOfPairs :: Int -> [Int] -> Bool
    isSumOfPairs n xs = isJust $ find (\(a, b) -> a + b == n) $ pairs xs
