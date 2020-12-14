module Advent2020.D1 (run, part1, part2) where

import Advent2020.Internal (pairs, runNumbers')
import Relude

run :: ([Int] -> Either Text Int) -> Text -> Either Text Int
run = runNumbers'

part1 :: [Int] -> Either Text Int
part1 entries = maybeToRight "no solution found" $ do
  (x, y) <- find (\(a, b) -> a + b == 2020) $ pairs entries
  return $ x * y

part2 :: [Int] -> Either Text Int
part2 entries = maybeToRight "no solution found" $ do
  (x, y, z) <- find (\(a, b, c) -> a + b + c == 2020) $ triples entries
  return $ x * y * z

triples :: [a] -> [(a, a, a)]
triples (x : xs) = ((\(y, z) -> (x, y, z)) <$> pairs xs) ++ triples xs
triples [] = []
