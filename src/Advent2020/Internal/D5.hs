module Advent2020.Internal.D5
  ( Partition (..),
    SeatSpec,
    parse,
    Seat,
    SeatPosition (..),
    parseSpec,
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

data SeatPosition t = SeatPosition
  { row :: t,
    column :: t
  }
  deriving (Show, Eq, Ord)

type Range = (Int, Int) -- Inclusive on lower bound, exclusive on higher bound.

type Seat = SeatPosition Int

parseSpec :: SeatSpec -> Either Text Seat
parseSpec spec = do
  let SeatPosition {..} = foldl' narrowRange seatRange spec
  row' <- rangeToInt row
  col' <- rangeToInt column
  return SeatPosition {row = row', column = col'}
  where
    seatRange :: SeatPosition Range
    seatRange =
      SeatPosition
        { row = (0, 128),
          column = (0, 8)
        }

    narrowRange :: SeatPosition Range -> Partition -> SeatPosition Range
    narrowRange p@SeatPosition {row = (low, high)} F = p {row = (low, high - narrow low high)}
    narrowRange p@SeatPosition {row = (low, high)} B = p {row = (low + narrow low high, high)}
    narrowRange p@SeatPosition {column = (low, high)} L = p {column = (low, high - narrow low high)}
    narrowRange p@SeatPosition {column = (low, high)} R = p {column = (low + narrow low high, high)}

    narrow :: Int -> Int -> Int
    narrow low high = (high - low) `div` 2

    rangeToInt :: Range -> Either Text Int
    rangeToInt r@(low, high) =
      if low + 1 == high
        then Right low
        else Left $ "invalid: attempted to turn range " <> show r <> " into integer"

seatID :: Seat -> Int
seatID SeatPosition {..} = row * 8 + column
