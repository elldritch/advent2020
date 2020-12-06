module Advent2020.D1 (run, part1, part2) where

import Advent2020.Internal (label, parseWith, parseWithPrettyErrors, readInt)
import Relude hiding (some)
import Text.Megaparsec (eof, hidden, someTill, (<?>))
import Text.Megaparsec.Char (digitChar, newline)

run :: Text -> ([Int] -> Maybe Int) -> Either Text Int
run contents runner = do
  xs <- label "parsing expenses" $ parse contents
  maybeToRight "no solution found" $ runner xs
  where
    parse :: Text -> Either Text [Int]
    parse = parseWithPrettyErrors $ parseWith readInt parseNumberLine `someTill` hidden eof

    parseNumberLine = digitChar `someTill` newline <?> "number"

part1 :: [Int] -> Maybe Int
part1 entries = do
  (x, y) <- find (\(a, b) -> a + b == 2020) $ pairs entries
  return $ x * y

part2 :: [Int] -> Maybe Int
part2 entries = do
  (x, y, z) <- find (\(a, b, c) -> a + b + c == 2020) $ triples entries
  return $ x * y * z

pairs :: [a] -> [(a, a)]
pairs (x : xs) = map (x,) xs ++ pairs xs
pairs [] = []

triples :: [a] -> [(a, a, a)]
triples (x : xs) = map (\(y, z) -> (x, y, z)) (pairs xs) ++ triples xs
triples [] = []
