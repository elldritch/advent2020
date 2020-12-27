module Advent2020.Internal.D11
  ( Grid (..),
    Position (..),
    parse,
    step,
    adjacent,
    firstVisibleSeat,
  )
where

import Advent2020.Internal (parseWithPrettyErrors)
import Data.List (delete, groupBy)
import Data.Map (assocs, lookup, mapWithKey)
import GHC.Show (Show (..))
import Relude hiding (show)
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec (eof, someTill)
import Text.Megaparsec.Char (char, newline)

newtype Grid = Grid
  { grid :: Map (Int, Int) Position
  }
  deriving (Eq)

instance Show Grid where
  show Grid {..} = toString $ unlines $ toText <$> ls
    where
      cells = sortOn (snd . fst) $ sortOn (fst . fst) $ assocs grid
      rows = groupBy (\((_, y), _) ((_, y'), _) -> y == y') cells
      ls = fmap (concatMap (\(_, p) -> show p)) rows

data Position
  = Floor
  | Empty
  | Occupied
  deriving (Eq)

instance Show Position where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

parse :: Text -> Either Text Grid
parse = parseWithPrettyErrors $ do
  rows <- row `someTill` eof
  return $ Grid $ fromList $ concatMap (\(y, xs) -> fmap (\(x, p) -> ((x, y), p)) xs) $ zip [0 ..] $ zip [0 ..] <$> rows
  where
    floorP = Floor <$ char '.'
    emptyP = Empty <$ char 'L'
    occupiedP = Occupied <$ char '#'
    row = (floorP <|> emptyP <|> occupiedP) `someTill` newline

adjacent :: Grid -> (Int, Int) -> [Position]
adjacent Grid {..} (x, y) = catMaybes $ (`lookup` grid) <$> neighbors
  where
    neighbors = delete (x, y) [(x + a, y + b) | a <- [-1, 0, 1], b <- [-1, 0, 1]]

firstVisibleSeat :: Grid -> (Int, Int) -> [Position]
firstVisibleSeat Grid {..} (x, y) = catMaybes $ firstSeatInVector <$> vectors
  where
    vectors = delete (0, 0) [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1]]

    outwardCells (dx, dy) = delete (x, y) (iterate (\(x', y') -> (x' + dx, y' + dy)) (x, y))

    firstSeatInVector vec =
      find (/= Floor) $ Unsafe.fromJust <$> takeWhile isJust ((`lookup` grid) <$> outwardCells vec)

step :: (Grid -> (Int, Int) -> [Position]) -> Int -> Grid -> Grid
step getNeighbors maxNeighbors g@Grid {..} = Grid $ mapWithKey stepPosition grid
  where
    stepPosition :: (Int, Int) -> Position -> Position
    stepPosition coordinate position
      | position == Empty && null neighbors = Occupied
      | position == Occupied && length neighbors >= maxNeighbors = Empty
      | otherwise = position
      where
        neighbors = filter (== Occupied) $ getNeighbors g coordinate
