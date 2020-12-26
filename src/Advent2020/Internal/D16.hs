module Advent2020.Internal.D16
  ( Range,
    Rules,
    Ticket,
    FieldName (..),
    FieldID (..),
    parse,
    invalidFields,
    possibleFields,
  )
where

import Advent2020.Internal (Parser, parseWith, parseWithPrettyErrors, readInt)
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import Data.Map (foldrWithKey, keys, unionWith)
import qualified Data.Map as Map
import Data.Set (insert, intersection, union)
import Relude
import Text.Megaparsec (chunk, eof, someTill, someTill_, try)
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, spaceChar)

type Range = (Int, Int)

type Rule = [Range]

type Rules = Map FieldName Rule

newtype FieldName = FieldName {unFieldName :: Text} deriving (IsString, Eq, Ord, Show)

type Ticket = Map FieldID Int

newtype FieldID = FieldID {unFieldID :: Int} deriving (Eq, Ord, Show)

parse :: Text -> Either Text (Rules, Ticket, NonEmpty Ticket)
parse = parseWithPrettyErrors $ do
  rules <- ruleP `someTill` newline
  _ <- chunk "your ticket:\n"
  mine <- ticketP
  _ <- newline
  _ <- chunk "nearby tickets:\n"
  others <- ticketP `NonEmpty.someTill` eof
  return (fromList rules, mine, others)
  where
    ruleP :: Parser (FieldName, [Range])
    ruleP = do
      name <- toText <$> (letterChar <|> spaceChar) `someTill` chunk ": "
      r1 <- rangeP
      _ <- chunk "or "
      r2 <- rangeP
      return (FieldName name, [r1, r2])

    rangeP :: Parser Range
    rangeP = do
      low <- parseWith readInt $ digitChar `someTill` char '-'
      high <- parseWith readInt $ digitChar `someTill` spaceChar
      return (low, high)

    ticketP :: Parser Ticket
    ticketP = do
      (fields, lastField) <- fieldP `someTill_` try lastFieldP
      return $ fromList $ zip (FieldID <$> [0 ..]) $ fields ++ [lastField]
      where
        fieldP = parseWith readInt $ digitChar `someTill` char ','
        lastFieldP = parseWith readInt $ digitChar `someTill` newline

-- | Returns true if the integer is within one of the ranges of the rule. Use infix as 'n `withinRange` rule'.
withinRule :: Int -> Rule -> Bool
withinRule n = or . fmap (withinRange n)

withinRange :: Int -> Range -> Bool
withinRange n (low, high) = n >= low && n <= high

invalidFields :: Rules -> Ticket -> Set FieldID
invalidFields rules = foldrWithKey (\fid v acc -> union acc $ if canBeValid v then mempty else one fid) mempty
  where
    canBeValid :: Int -> Bool
    canBeValid x = or $ withinRule x <$> toList rules

possibleFields' :: Rules -> Ticket -> Map FieldID (Set FieldName)
possibleFields' rs = Map.map possibleNamesOf
  where
    possibleNamesOf :: Int -> Set FieldName
    possibleNamesOf n = foldrWithKey (accumulatePossibleNames n) mempty rs

    accumulatePossibleNames :: Int -> FieldName -> Rule -> Set FieldName -> Set FieldName
    accumulatePossibleNames n field rule acc = if n `withinRule` rule then insert field acc else acc

possibleFields :: Rules -> NonEmpty Ticket -> Map FieldID (Set FieldName)
possibleFields rs ts = foldr intersectPossibleFields allFieldsPossible ts
  where
    allFieldsPossible :: Map FieldID (Set FieldName)
    allFieldsPossible = Map.map (const $ fromList $ keys rs) $ head ts

    intersectPossibleFields :: Ticket -> Map FieldID (Set FieldName) -> Map FieldID (Set FieldName)
    intersectPossibleFields t fs = unionWith intersection (possibleFields' rs t) fs
