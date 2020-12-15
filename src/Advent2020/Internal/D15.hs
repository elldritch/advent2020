module Advent2020.Internal.D15
  ( parse,
    Game (..),
    spoken,
  )
where

import Advent2020.Internal (parseWith, parseWithPrettyErrors, readInt)
import Data.Map (insert, lookup)
import Relude
import Relude.Unsafe (fromJust)
import Text.Megaparsec (eof, someTill)
import Text.Megaparsec.Char (char, digitChar, newline)

parse :: Text -> Either Text [Int]
parse = parseWithPrettyErrors $ parseWith readInt (digitChar `someTill` (char ',' <|> newline)) `someTill` eof

data Game = Game
  { turnLastSpoken :: Map Int Int,
    next :: Int,
    currentTurn :: Int
  }
  deriving (Show)

spoken :: [Int] -> [Game]
spoken starting = iterate speak startingGame
  where
    starting' = fromJust $ nonEmpty $ zip [1 ..] starting
    (currentTurn', next') = last starting'

    startingGame =
      Game
        { turnLastSpoken = foldl' (\m (i, v) -> insert v i m) mempty $ init starting',
          next = next',
          currentTurn = currentTurn'
        }

speak :: Game -> Game
speak g@Game {..} = case lookup next turnLastSpoken of
  Just t -> g' {next = currentTurn - t}
  Nothing -> g' {next = 0}
  where
    g' =
      g
        { currentTurn = currentTurn + 1,
          turnLastSpoken = insert next currentTurn turnLastSpoken
        }
