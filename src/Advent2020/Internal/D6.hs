module Advent2020.Internal.D6 (Group, parse) where

import Advent2020.Internal (Parser, parseWithPrettyErrors)
import Relude
import Text.Megaparsec (eof, sepEndBy1)
import Text.Megaparsec.Char (letterChar, newline)

type Group = [Set Char]

parse :: Text -> Either Text [Group]
parse = parseWithPrettyErrors $ groupParser `sepEndBy1` newline <* eof
  where
    responseParser :: Parser (Set Char)
    responseParser = fromList <$> some letterChar

    groupParser :: Parser Group
    groupParser = responseParser `sepEndBy1` newline
