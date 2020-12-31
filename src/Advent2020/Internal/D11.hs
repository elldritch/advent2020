module Advent2020.Internal.D11
  ( Grid (..),
    Position (..),
    parse,
    step,
    adjacent,
    firstVisibleSeat,
  )
where

import Advent2020.Internal (Grid (..), gridMap, parseGrid, parseWithPrettyErrors)
import Control.Lens (over, (^.))
import Data.List (delete)
import Data.Map (lookup, mapWithKey)
import GHC.Show (Show (..))
import Relude hiding (show)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (char)

data Position
  = Floor
  | Empty
  | Occupied
  deriving (Eq)

instance Show Position where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

parse :: Text -> Either Text (Grid Position)
parse = parseWithPrettyErrors $ parseGrid cell <* eof
  where
    floorP = Floor <$ char '.'
    emptyP = Empty <$ char 'L'
    occupiedP = Occupied <$ char '#'
    cell = floorP <|> emptyP <|> occupiedP

adjacent :: Grid Position -> (Int, Int) -> [Position]
adjacent g (x, y) = catMaybes $ (`lookup` (g ^. gridMap)) <$> neighbors
  where
    neighbors = delete (x, y) [(x + a, y + b) | a <- [-1, 0, 1], b <- [-1, 0, 1]]

firstVisibleSeat :: Grid Position -> (Int, Int) -> [Position]
firstVisibleSeat g (x, y) = catMaybes $ firstSeatInVector <$> vectors
  where
    vectors = delete (0, 0) [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1]]

    outwardCells (dx, dy) = delete (x, y) (iterate (\(x', y') -> (x' + dx, y' + dy)) (x, y))

    firstSeatInVector vec =
      find (/= Floor) $ catMaybes $ takeWhile isJust ((`lookup` (g ^. gridMap)) <$> outwardCells vec)

step :: (Grid Position -> (Int, Int) -> [Position]) -> Int -> Grid Position -> Grid Position
step getNeighbors maxNeighbors g = over gridMap (mapWithKey stepPosition) g
  where
    stepPosition :: (Int, Int) -> Position -> Position
    stepPosition coordinate position
      | position == Empty && null neighbors = Occupied
      | position == Occupied && length neighbors >= maxNeighbors = Empty
      | otherwise = position
      where
        neighbors = filter (== Occupied) $ getNeighbors g coordinate
