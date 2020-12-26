module Advent2020.D2 (run, part1, part2) where

import Advent2020.Internal (label)
import Advent2020.Internal.D2 (Password (..), parse)
import qualified Data.Text as Text
import Relude

run :: (Password -> Bool) -> Text -> Either Text Int
run isValid contents = do
  passwords <- label "parsing passwords" $ parse contents
  return $ sum $ fromEnum . isValid <$> passwords

part1 :: Password -> Bool
part1 Password {..} = count >= a && count <= b
  where
    countLetter :: Char -> Text -> Int
    countLetter c t = Text.length $ Text.filter (== c) t

    count = countLetter letter password

part2 :: Password -> Bool
part2 Password {..} = letterAt a `xor` letterAt b
  where
    letterAt i = letter == password `Text.index` (i - 1)
