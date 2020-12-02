module Advent2020.D2 (run, part1, part2) where

import Advent2020.Internal.D2 (Password (..), parse)
import Relude hiding (max, min, some)
import Relude.Unsafe ((!!))
import Text.Megaparsec (errorBundlePretty)

run :: Text -> (Password -> Bool) -> Int
run contents isValid = sum $ map fromEnum valids
  where
    passwords = case parse contents of
      Right ps -> ps
      Left err -> error $ toText $ errorBundlePretty err
    valids = map isValid passwords

part1 :: Password -> Bool
part1 Password {..} = count >= a && count <= b
  where
    countLetter :: Char -> String -> Int
    countLetter l s = foldr (\c n -> if c == l then n + 1 else n) 0 s

    count = countLetter letter $ toString password

part2 :: Password -> Bool
part2 Password {..} = letterAt a `xor` letterAt b
  where
    letterAt i = password !! (i - 1) == letter
