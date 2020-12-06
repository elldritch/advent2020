module Advent2020.D2 (run, part1, part2) where

import Advent2020.Internal (gather')
import Advent2020.Internal.D2 (Password (..), parse)
import Relude

run :: Text -> (Password -> Either Text Bool) -> Either Text Int
run contents isValid = do
  passwords <- parse contents
  valids <- gather' $ map isValid passwords
  return $ sum $ map fromEnum valids

part1 :: Password -> Either Text Bool
part1 Password {..} = Right $ count >= a && count <= b
  where
    countLetter :: Char -> String -> Int
    countLetter l s = foldr (\c n -> if c == l then n + 1 else n) 0 s

    count = countLetter letter password

part2 :: Password -> Either Text Bool
part2 Password {..} = do
  a' <- letterAt a
  b' <- letterAt b
  return $ a' `xor` b'
  where
    letterAt i = maybeToRight "index out of password bounds" $ do
      l <- password !!? (i - 1)
      return $ l == letter
