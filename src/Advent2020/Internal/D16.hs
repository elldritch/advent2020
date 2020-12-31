module Advent2020.Internal.D16
  ( Range,
    Rules,
    Ticket,
    FieldName (..),
    FieldID (..),
    parse,
    invalidFields,
    possibleFieldNames,
    chooseNamesUntilAmbiguous,
  )
where

import Advent2020.Internal (Parser, fixed, integralP, parseWithPrettyErrors, symbol, unsafeNonEmpty, wordP)
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import Data.Map (foldrWithKey, unionWith)
import qualified Data.Map as Map
import Data.Set (intersection, union, (\\))
import qualified Data.Set as Set
import Relude
import Relude.Extra.Map
import Text.Megaparsec (eof, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (newline)

type Range = (Integer, Integer)

type Rule = [Range]

type Rules = Map FieldName Rule

newtype FieldName = FieldName {unFieldName :: Text} deriving (IsString, Eq, Ord, Show)

type Ticket = Map FieldID Integer

newtype FieldID = FieldID {unFieldID :: Int} deriving (Eq, Ord, Show)

parse :: Text -> Either Text (Rules, Ticket, NonEmpty Ticket)
parse = parseWithPrettyErrors $ do
  rules <- ruleP `sepEndBy1` newline
  _ <- newline
  _ <- symbol "your ticket:\n"
  mine <- ticketP <* newline
  _ <- newline
  _ <- symbol "nearby tickets:\n"
  others <- ticketP `NonEmpty.sepEndBy1` newline
  _ <- eof
  return (fromList rules, mine, others)
  where
    ruleP :: Parser (FieldName, [Range])
    ruleP = do
      name <- unwords <$> some wordP
      _ <- symbol ":"
      r1 <- rangeP
      _ <- symbol "or"
      r2 <- rangeP
      return (FieldName name, [r1, r2])

    rangeP :: Parser Range
    rangeP = do
      low <- integralP
      _ <- symbol "-"
      high <- integralP
      return (low, high)

    ticketP :: Parser Ticket
    ticketP = fromList . zip (FieldID <$> [0 ..]) <$> (integralP `sepBy1` symbol ",")

-- | Returns true if the integer is within one of the ranges of the rule. Use infix as 'n `withinRange` rule'.
withinRule :: Integer -> Rule -> Bool
withinRule n = or . fmap (withinRange n)

withinRange :: Integer -> Range -> Bool
withinRange n (low, high) = n >= low && n <= high

invalidFields :: Rules -> Ticket -> Set FieldID
invalidFields rules = foldrWithKey (\fid v acc -> union acc $ if canBeValid v then mempty else one fid) mempty
  where
    canBeValid :: Integer -> Bool
    canBeValid x = or $ withinRule x <$> toList rules

possibleFieldNames' :: Rules -> Ticket -> Map FieldID (Set FieldName)
possibleFieldNames' rules = fmap possibleNamesForValue
  where
    possibleNamesForValue :: Integer -> Set FieldName
    possibleNamesForValue n = foldrWithKey (accumulatePossibleNames n) mempty rules

    accumulatePossibleNames :: Integer -> FieldName -> Rule -> Set FieldName -> Set FieldName
    accumulatePossibleNames n field rule acc = if n `withinRule` rule then Set.insert field acc else acc

possibleFieldNames :: Rules -> NonEmpty Ticket -> Map FieldID (Set FieldName)
possibleFieldNames rules tickets = foldr intersectPossibleFields allFieldsPossible tickets
  where
    allFieldsPossible :: Map FieldID (Set FieldName)
    allFieldsPossible = fromList (keys rules) <$ head tickets

    intersectPossibleFields :: Ticket -> Map FieldID (Set FieldName) -> Map FieldID (Set FieldName)
    intersectPossibleFields ticket fields = unionWith intersection (possibleFieldNames' rules ticket) fields

chooseNamesUntilAmbiguous :: Rules -> NonEmpty Ticket -> Map FieldID FieldName
chooseNamesUntilAmbiguous rules tickets = fixed (narrow possible) mempty
  where
    possible = possibleFieldNames rules tickets

    narrow :: Map FieldID (Set FieldName) -> Map FieldID FieldName -> Map FieldID FieldName
    narrow possibilities choices = case nextChoice of
      Nothing -> choices
      Just (fieldID, fieldName) -> insert fieldID (head $ unsafeNonEmpty $ toList fieldName) choices
      where
        chosen :: Set FieldName
        chosen = fromList $ elems choices

        possibilities' :: Map FieldID (Set FieldName)
        possibilities' = (\\ chosen) <$> possibilities

        nextChoice :: Maybe (FieldID, Set FieldName)
        nextChoice = find ((== 1) . size . snd) (Map.toList possibilities')
