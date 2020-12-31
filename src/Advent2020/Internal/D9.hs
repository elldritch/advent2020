module Advent2020.Internal.D9 (findWeakNumber, findWeakSet) where

import Advent2020.Internal (largest, pairs, smallest, windows)
import Relude

findWeakNumber :: Int -> [Int] -> Either Text Int
findWeakNumber preambleSize ns =
  fst
    <$> maybeToRight
      "could not find weak number"
      (find (\(n, w) -> not $ isSumOfPairs n w) $ zip (drop preambleSize ns) $ windows preambleSize ns)
  where
    isSumOfPairs :: Int -> [Int] -> Bool
    isSumOfPairs n xs = isJust $ find (\(a, b) -> a + b == n) $ pairs xs

findWeakSet :: Int -> [Int] -> Either Text Int
findWeakSet preambleSize ns = do
  weakNumber <- findWeakNumber preambleSize ns
  let ws = (`windows` ns) `concatMap` [2 ..]
  weakSet <- maybeToRight "could not find weak set" $ find (\w -> sum w == weakNumber) ws
  s <- maybeToRight "weak set is empty" $ viaNonEmpty smallest weakSet
  l <- maybeToRight "weak set is empty" $ viaNonEmpty largest weakSet
  return $ s + l
