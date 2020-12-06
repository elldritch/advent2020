module Advent2020.D2 (run, part1, part2) where

import Advent2020.Internal (label, mapE')
import Advent2020.Internal.D2 (Password (..), parse)
import Relude

run :: Text -> (Password -> Either Text Bool) -> Either Text Int
run contents isValid = do
  passwords <- label "parsing passwords" $ parse contents
  valids <- label "computing valid passwords" $ mapE' isValid passwords
  return $ sum $ map fromEnum valids

part1 :: Password -> Either Text Bool
part1 Password {..} = Right $ count >= a && count <= b
  where
    countLetter :: Char -> String -> Int
    countLetter l s = foldr (\c n -> if c == l then n + 1 else n) 0 s

    count = countLetter letter password

part2 :: Password -> Either Text Bool
part2 Password {..} = do
  a' <- label "getting first letter" $ letterAt a
  b' <- label "getting second letter" $ letterAt b
  return $ a' `xor` b'
  where
    letterAt i = maybeToRight ("index " <> show i <> " out of bounds of text " <> show password) $ do
      l <- password !!? (i - 1)
      return $ l == letter
