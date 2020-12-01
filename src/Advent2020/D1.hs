module Advent2020.D1 (run, part1, part2) where

import Relude
import Relude.Unsafe (fromJust, read)

run :: FilePath -> ([Int] -> Int) -> IO Int
run file runner = do
  contents <- readFileText file
  let ls = lines contents
  let xs = map ((\x -> read x :: Int) . toString) ls
  return $ runner xs

part1 :: [Int] -> Int
part1 entries = let (x, y) = fromJust $ find (\(a, b) -> a + b == 2020) (pairs entries) in x * y

part2 :: [Int] -> Int
part2 entries = let (x, y, z) = fromJust $ find (\(a, b, c) -> a + b + c == 2020) (triples entries) in x * y * z

pairs :: [a] -> [(a, a)]
pairs (x : xs) = map (x,) xs ++ pairs xs
pairs [] = []

triples :: [a] -> [(a, a, a)]
triples (x : xs) = map (\(y, z) -> (x, y, z)) (pairs xs) ++ triples xs
triples [] = []
