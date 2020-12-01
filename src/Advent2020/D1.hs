module Advent2020.D1 (runPart1, part1) where

import Relude
import Relude.Unsafe (fromJust, read)

runPart1 :: FilePath -> IO Int
runPart1 file = do
  contents <- readFileText file
  let ls = lines contents
  let xs = map ((\x -> read x :: Int) . toString) ls
  return $ part1 xs

part1 :: [Int] -> Int
part1 entries = let (x, y) = fromJust $ find (\(a, b) -> a + b == 2020) (pairs entries) in x * y

pairs :: [a] -> [(a, a)]
pairs (x : xs) = map (x,) xs ++ pairs xs
pairs [] = []
