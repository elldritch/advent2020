module Advent2020.Internal.D11
  ( Grid,
    Position (..),
    parse,
    step,
  )
where

import Advent2020.Internal (parseWithPrettyErrors)
import Data.List (delete)
import Data.Map (lookup, mapWithKey)
import Relude
import Text.Megaparsec (eof, someTill)
import Text.Megaparsec.Char (char, newline)

type Grid = Map (Int, Int) Position

data Position
  = Floor
  | Empty
  | Occupied
  deriving (Show, Eq)

parse :: Text -> Either Text Grid
parse = parseWithPrettyErrors $ do
  rows <- row `someTill` eof
  return $ fromList $ concatMap (\(y, xs) -> fmap (\(x, p) -> ((x, y), p)) xs) $ zip [0 ..] $ zip [0 ..] <$> rows
  where
    floorP = void (char '.') >> return Floor
    emptyP = void (char 'L') >> return Empty
    occupiedP = void (char '#') >> return Occupied
    row = (floorP <|> emptyP <|> occupiedP) `someTill` newline

adjacent :: Grid -> (Int, Int) -> [Position]
adjacent grid (x, y) = catMaybes $ (`lookup` grid) <$> neighbors
  where
    neighbors = delete (x, y) [(x + a, y + b) | a <- [-1, 0, 1], b <- [-1, 0, 1]]

step :: Grid -> Grid
step grid = mapWithKey stepPosition grid
  where
    stepPosition :: (Int, Int) -> Position -> Position
    stepPosition coordinate position
      | position == Empty && null neighbors = Occupied
      | position == Occupied && length neighbors >= 4 = Empty
      | otherwise = position
      where
        neighbors = filter (== Occupied) $ adjacent grid coordinate
