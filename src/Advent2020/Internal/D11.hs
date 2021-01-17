module Advent2020.Internal.D11
  ( Grid (..),
    Square (..),
    parse,
    step,
    adjacent,
    firstVisibleSeat,
  )
where

import Advent2020.Internal (Grid (..), Position, gridMap, neighbors, parseGrid, parseWithPrettyErrors)
import Control.Lens (over, (^.))
import Data.List (delete)
import Data.Map (lookup, mapWithKey)
import GHC.Show (Show (..))
import Relude hiding (show)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (char)

data Square
  = Floor
  | Empty
  | Occupied
  deriving (Eq)

instance Show Square where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

parse :: Text -> Either Text (Grid Square)
parse = parseWithPrettyErrors $ parseGrid cell <* eof
  where
    floorP = Floor <$ char '.'
    emptyP = Empty <$ char 'L'
    occupiedP = Occupied <$ char '#'
    cell = floorP <|> emptyP <|> occupiedP

adjacent :: Grid Square -> Position -> [Square]
adjacent g (x, y) = catMaybes $ (`lookup` (g ^. gridMap)) <$> neighbors (x, y)

firstVisibleSeat :: Grid Square -> Position -> [Square]
firstVisibleSeat g (x, y) = catMaybes $ firstSeatInVector <$> outwards
  where
    outwards = delete (0, 0) [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1]]

    inDirection (dx, dy) = delete (x, y) (iterate (\(x', y') -> (x' + dx, y' + dy)) (x, y))

    firstSeatInVector vec =
      find (/= Floor) $ catMaybes $ takeWhile isJust ((`lookup` (g ^. gridMap)) <$> inDirection vec)

step :: (Grid Square -> Position -> [Square]) -> Int -> Grid Square -> Grid Square
step getNeighbors maxNeighbors g = over gridMap (mapWithKey f) g
  where
    f :: Position -> Square -> Square
    f p s
      | s == Empty && null ns = Occupied
      | s == Occupied && length ns >= maxNeighbors = Empty
      | otherwise = s
      where
        ns = filter (== Occupied) $ getNeighbors g p
