module Advent2020.Internal.D15
  ( parse,
    Game (..),
    spoken,
    nth,
  )
where

import Advent2020.Internal (integralP, parseWithPrettyErrors)
import Control.Monad.Combinators.NonEmpty (sepBy1)
import qualified Data.List.NonEmpty as NonEmpty
import Relude
import Relude.Extra.Map
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (char, newline)

parse :: Text -> Either Text (NonEmpty Integer)
parse = parseWithPrettyErrors $ integralP `sepBy1` char ',' <* (newline >> eof)

data Game = Game
  { turnLastSpoken :: Map Integer Integer,
    next :: Integer,
    currentTurn :: Integer
  }
  deriving (Show)

spoken :: NonEmpty Integer -> [Game]
spoken starting = iterate speak startingGame
  where
    starting' = NonEmpty.zip (1 :| [2 ..]) starting
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

nth :: Integer -> [Game] -> Integer
nth n = next . Unsafe.fromJust . find ((== n) . currentTurn)
