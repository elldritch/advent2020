module Advent2020.Internal.D16 (Range, Rules, Ticket, parse) where

import Advent2020.Internal (Parser, parseWith, parseWithPrettyErrors, readInt)
import Relude
import Text.Megaparsec (try, eof, chunk, someTill, someTill_)
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, spaceChar)

type Range = (Int, Int)

type Rules = Map Text [Range]

type Ticket = Map Text Int

parse :: Text -> Either Text (Rules, Ticket, [Ticket])
parse = parseWithPrettyErrors $ do
  rules <- ruleP `someTill` newline
  _ <- chunk "your ticket:\n"
  mine <- ticketP
  _ <- newline
  _ <- chunk "nearby tickets:\n"
  others <- ticketP `someTill` eof
  return (fromList rules, mine, others)
  where
    ruleP :: Parser (Text, [Range])
    ruleP = do
      name <- toText <$> (letterChar <|> spaceChar) `someTill` chunk ": "
      r1 <- rangeP
      _ <- chunk "or "
      r2 <- rangeP
      return (name, [r1, r2])

    rangeP :: Parser Range
    rangeP = do
      low <- parseWith readInt $ digitChar `someTill` char '-'
      high <- parseWith readInt $ digitChar `someTill` spaceChar
      return (low, high)

    ticketP :: Parser Ticket
    ticketP = do
      (fields, lastField) <- fieldP `someTill_` try lastFieldP
      return $ fromList $ zip (show <$> ([0 ..] :: [Int])) $ fields ++ [lastField]
      where
        fieldP = parseWith readInt $ digitChar `someTill` char ','
        lastFieldP = parseWith readInt $ digitChar `someTill` newline
