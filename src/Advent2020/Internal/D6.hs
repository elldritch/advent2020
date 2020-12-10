module Advent2020.Internal.D6 (Group, parse) where

import Advent2020.Internal (Parser, parseWithPrettyErrors)
import Relude
import Text.Megaparsec (eof, hidden, someTill)
import Text.Megaparsec.Char (letterChar, newline)

type Group = [Set Char]

parse :: Text -> Either Text [Group]
parse = parseWithPrettyErrors $ groupParser `someTill` hidden eof
  where
    responseParser :: Parser (Set Char)
    responseParser = do
      chars <- letterChar `someTill` newline
      return $ fromList chars

    groupParser :: Parser Group
    groupParser = responseParser `someTill` (void newline <|> eof)
