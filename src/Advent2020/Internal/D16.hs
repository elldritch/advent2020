module Advent2020.Internal.D16 (Range, Rules, Ticket, parse, invalidTickets) where

import Advent2020.Internal (Parser, parseWith, parseWithPrettyErrors, readInt)
import Data.Map (foldrWithKey)
import Data.Set (union)
import Relude
import Text.Megaparsec (chunk, eof, someTill, someTill_, try)
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

invalidTickets :: Rules -> [Ticket] -> [(Ticket, Set Text)]
invalidTickets rules = foldl' collectInvalidTickets []
  where
    withinRange :: Range -> Int -> Bool
    withinRange (low, high) x = x >= low && x <= high

    canBeValid :: Int -> Bool
    canBeValid x = or $ (\ranges -> or $ (`withinRange` x) <$> ranges) <$> toList rules

    invalidFields :: Ticket -> Set Text
    invalidFields ticket = foldrWithKey (\k v fs -> union fs $ if canBeValid v then mempty else one k) mempty ticket

    collectInvalidTickets :: [(Ticket, Set Text)] -> Ticket -> [(Ticket, Set Text)]
    collectInvalidTickets acc ticket = acc ++ [(ticket, fs) | not (null fs)]
      where
        fs = invalidFields ticket
