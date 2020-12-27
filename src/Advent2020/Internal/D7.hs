module Advent2020.Internal.D7 (Rule (..), parse) where

import Advent2020.Internal (Parser, parseWith, parseWithPrettyErrors, readInt)
import Relude
import Text.Megaparsec (chunk, eof, someTill)
import Text.Megaparsec.Char (char, digitChar, hspace1, letterChar, newline, punctuationChar)

data Rule = Rule
  { color :: Text,
    contains :: Map Text Int
  }
  deriving (Show, Eq)

parse :: Text -> Either Text [Rule]
parse = parseWithPrettyErrors $ parseRule `someTill` eof
  where
    parseWord :: Parser Text
    parseWord = toText <$> letterChar `someTill` (hspace1 <|> void punctuationChar)

    parseTarget :: Parser Text
    parseTarget = do
      firstWord <- parseWord
      secondWord <- parseWord
      _ <- chunk "bags"
      return $ firstWord <> " " <> secondWord

    parseContained :: Parser (Map Text Int)
    parseContained = do
      n <- parseWith readInt $ digitChar `someTill` hspace1
      firstWord <- parseWord
      secondWord <- parseWord
      _ <- chunk $ if n == 1 then "bag" else "bags"
      _ <- optional $ chunk ", "
      return $ one (firstWord <> " " <> secondWord, n)

    parseRule :: Parser Rule
    parseRule = do
      color <- parseTarget
      _ <- chunk " contain "
      contains <- (chunk "no other bags." >> mempty) <|> (mconcat <$> (parseContained `someTill` char '.'))
      _ <- newline
      return Rule {..}
