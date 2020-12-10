module Advent2020.D5 (part1, Partition (..), SeatSpec, parse) where

import Advent2020.Internal (gather', Parser, parseWithPrettyErrors)
import Relude
import Text.Megaparsec (count, eof, hidden, someTill)
import Text.Megaparsec.Char (char, spaceChar)

part1 :: Text -> Either Text Int
part1 contents = do
  specs <- parse contents
  coords <- gather' $ specToCoords <$> specs
  let seatIDs = seatID <$> coords
  return $ foldr maxSeatID 0 seatIDs

data Partition
  = F
  | B
  | L
  | R
  deriving (Show, Eq)

type SeatSpec = [Partition]

parse :: Text -> Either Text [SeatSpec]
parse = parseWithPrettyErrors $ do
  parseSpec `someTill` hidden eof
  where
    parseSpec :: Parser SeatSpec
    parseSpec = do
      xs <- count 7 ((char 'F' >> return F) <|> (char 'B' >> return B))
      ys <- count 3 ((char 'L' >> return L) <|> (char 'R' >> return R))
      _ <- spaceChar
      return $ xs <> ys

data Coordinates t = Coords
  { row :: t,
    column :: t
  }
  deriving (Show)

type Range = (Int, Int)

specToCoords :: SeatSpec -> Either Text (Coordinates Int)
specToCoords spec = do
  let Coords {..} = foldr narrowRange seatRange (reverse spec)
  row' <- rangeToInt row
  col' <- rangeToInt column
  return $ trace ("coords: " <> show (Coords {row = row', column = col'})) Coords {row = row', column = col'}
  where
    seatRange :: Coordinates Range
    seatRange =
      Coords
        { row = (0, 127),
          column = (0, 7)
        }

    narrowRange :: Partition -> Coordinates Range -> Coordinates Range
    narrowRange F c@Coords {row = (low, high)} = c {row = (traceShowWith "F row=" (low, high - ((high - low) `ceilDiv` 2)))}
    narrowRange B c@Coords {row = (low, high)} = c {row = (traceShowWith "B row=" (low + ((high - low) `ceilDiv` 2), high))}
    narrowRange L c@Coords {column = (low, high)} = c {column = (traceShowWith "L col=" (low, high - ((high - low) `ceilDiv` 2)))}
    narrowRange R c@Coords {column = (low, high)} = c {column = (traceShowWith "R col="(low + ((high - low) `ceilDiv` 2), high))}

ceilDiv :: Int -> Int -> Int
ceilDiv x y = trace ("ceilDiv diff between high/low=" <> (show x) <> " result=" <> show(if x `mod` y == 0 then d else d + 1)) (if x `mod` y == 0 then d else d + 1)
  where
    d = x `div` y

rangeToInt :: Range -> Either Text Int
rangeToInt r@(low, high) =
  if low == high
    then Right low
    else Left $ "invalid: attempted to turn range " <> show r <> " into integer"

seatID :: Coordinates Int -> Int
seatID Coords {..} = row * 8 + column

maxSeatID :: Int -> Int -> Int
maxSeatID x y = if x >= y then x else y
