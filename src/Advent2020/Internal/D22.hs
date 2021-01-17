module Advent2020.Internal.D22
  ( Deck,
    Game,
    Player (..),
    parse,
    play,
    play',
    score,
  )
where

import Advent2020.Internal (integralP, parseWithPrettyErrors, symbol, unsafeNonEmpty)
import Control.Monad.Combinators.NonEmpty (some)
import Data.Set (insert, member)
import Data.Tuple.Extra (both)
import Relude hiding (some)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (newline)

type Deck = NonEmpty Int

type Game = (Deck, Deck)

data Player = One | Two
  deriving (Show, Eq)

parse :: Text -> Either Text Game
parse = parseWithPrettyErrors $ do
  d1 <- parseDeck "1"
  _ <- newline
  d2 <- parseDeck "2"
  eof
  return (d1, d2)
  where
    parseDeck player = do
      _ <- symbol "Player"
      _ <- symbol player
      _ <- symbol ":"
      _ <- newline
      some $ integralP <* newline

step :: Game -> Either (Player, Deck) Game
step (d1, d2) = do
  d1'' <- maybeToRight (Two, unsafeNonEmpty d2') $ nonEmpty d1'
  d2'' <- maybeToRight (One, unsafeNonEmpty d1') $ nonEmpty d2'
  return (d1'', d2'')
  where
    c1 = head d1
    c2 = head d2
    d1' = tail d1 <> if c1 > c2 then [c1, c2] else mempty
    d2' = tail d2 <> if c2 > c1 then [c2, c1] else mempty

play :: Game -> (Player, Deck)
play g = case step g of
  Right g' -> play g'
  Left i -> i

score :: Deck -> Int
score d = foldr (\(n, card) acc -> n * card + acc) 0 $ zip [1 ..] $ reverse $ toList d

step' :: Set Game -> Game -> Either (Player, Deck) (Set Game, Game)
step' seen g@(d1, d2) =
  if member g seen
    then Left (One, d1)
    else do
      d1Next <- maybeToRight (Two, unsafeNonEmpty d2Played) $ nonEmpty d1Played
      d2Next <- maybeToRight (One, unsafeNonEmpty d1Played) $ nonEmpty d2Played
      return (insert g seen, (d1Next, d2Next))
  where
    c1 = head d1
    c2 = head d2
    d1Drawn = tail d1
    d2Drawn = tail d2
    winner
      -- Case: play sub-game, victor wins.
      | c1 <= length d1Drawn && c2 <= length d2Drawn =
        fst $ play' (both unsafeNonEmpty (take c1 d1Drawn, take c2 d2Drawn))
      -- Case: no sub-game possible, higher value wins.
      | c1 > c2 = One
      | otherwise = Two
    (d1Played, d2Played) = case winner of
      One -> (d1Drawn <> [c1, c2], d2Drawn)
      Two -> (d1Drawn, d2Drawn <> [c2, c1])

play' :: Game -> (Player, Deck)
play' = f mempty
  where
    f :: Set Game -> Game -> (Player, Deck)
    f seen g = case step' seen g of
      Right (seen', g') -> f seen' g'
      Left l -> l
