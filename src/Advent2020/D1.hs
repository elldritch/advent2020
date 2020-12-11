module Advent2020.D1 (run, part1, part2) where

import Advent2020.Internal (label, pairs, parseNumbers)
import Relude

run :: Text -> ([Int] -> Maybe Int) -> Either Text Int
run contents runner = do
  xs <- label "parsing expenses" $ parseNumbers contents
  maybeToRight "no solution found" $ runner xs

part1 :: [Int] -> Maybe Int
part1 entries = do
  (x, y) <- find (\(a, b) -> a + b == 2020) $ pairs entries
  return $ x * y

part2 :: [Int] -> Maybe Int
part2 entries = do
  (x, y, z) <- find (\(a, b, c) -> a + b + c == 2020) $ triples entries
  return $ x * y * z

triples :: [a] -> [(a, a, a)]
triples (x : xs) = map (\(y, z) -> (x, y, z)) (pairs xs) ++ triples xs
triples [] = []
