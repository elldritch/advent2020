module Advent2020.Internal.D5
  ( Partition (..),
    SeatSpec,
    parse,
    Position (..),
    specToPosition,
    seatID,
  )
where

import Advent2020.Internal (Parser, parseWithPrettyErrors)
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import Relude
import Text.Megaparsec (count, eof)
import Text.Megaparsec.Char (char, newline)

data Partition
  = F
  | B
  | L
  | R
  deriving (Show, Eq)

type SeatSpec = [Partition]

parse :: Text -> Either Text (NonEmpty SeatSpec)
parse = parseWithPrettyErrors $ specParser `NonEmpty.sepEndBy1` newline <* eof
  where
    specParser :: Parser SeatSpec
    specParser = do
      xs <- count 7 $ (F <$ char 'F') <|> (B <$ char 'B')
      ys <- count 3 $ (L <$ char 'L') <|> (R <$ char 'R')
      return $ xs <> ys

data Position t = Position
  { row :: t,
    column :: t
  }
  deriving (Show, Eq, Ord)

type Range = (Int, Int) -- Inclusive on lower bound, exclusive on higher bound.

specToPosition :: SeatSpec -> Either Text (Position Int)
specToPosition spec = do
  let Position {..} = foldl' narrowRange seatRange spec
  row' <- rangeToInt row
  col' <- rangeToInt column
  return Position {row = row', column = col'}
  where
    seatRange :: Position Range
    seatRange =
      Position
        { row = (0, 128),
          column = (0, 8)
        }

    narrowRange :: Position Range -> Partition -> Position Range
    narrowRange p@Position {row = (low, high)} F = p {row = (low, high - narrow low high)}
    narrowRange p@Position {row = (low, high)} B = p {row = (low + narrow low high, high)}
    narrowRange p@Position {column = (low, high)} L = p {column = (low, high - narrow low high)}
    narrowRange p@Position {column = (low, high)} R = p {column = (low + narrow low high, high)}

    narrow :: Int -> Int -> Int
    narrow low high = (high - low) `div` 2

    rangeToInt :: Range -> Either Text Int
    rangeToInt r@(low, high) =
      if low + 1 == high
        then Right low
        else Left $ "invalid: attempted to turn range " <> show r <> " into integer"

seatID :: Position Int -> Int
seatID Position {..} = row * 8 + column
