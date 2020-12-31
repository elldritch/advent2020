module Advent2020.Internal.D7 (Rule (..), parse) where

import Advent2020.Internal (Parser, integralP, parseWithPrettyErrors, symbol, wordP)
import Relude
import Text.Megaparsec (eof, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (char, newline)

data Rule = Rule
  { color :: Text,
    contains :: Map Text Int
  }
  deriving (Show, Eq)

parse :: Text -> Either Text [Rule]
parse = parseWithPrettyErrors $ parseRule `sepEndBy1` newline <* eof
  where
    parseTarget :: Parser Text
    parseTarget = do
      firstWord <- wordP
      secondWord <- wordP
      _ <- symbol "bags"
      return $ firstWord <> " " <> secondWord

    parseContained :: Parser (Map Text Int)
    parseContained = do
      n <- integralP
      firstWord <- wordP
      secondWord <- wordP
      _ <- symbol $ if n == 1 then "bag" else "bags"
      return $ one (firstWord <> " " <> secondWord, n)

    parseRule :: Parser Rule
    parseRule = do
      color <- parseTarget
      _ <- symbol "contain"
      contains <- (symbol "no other bags" >> mempty) <|> (mconcat <$> (parseContained `sepBy1` symbol ","))
      _ <- char '.'
      return Rule {..}
