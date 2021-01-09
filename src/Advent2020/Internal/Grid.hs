module Advent2020.Internal.Grid
  ( Grid (..),
    gridHeight,
    gridWidth,
    gridMap,
    parseGrid,
    showGrid,
    Sides (..),
    edges,
  )
where

import Advent2020.Internal.Parse (Parser)
import Control.Lens (makeLenses, over, view, (^.))
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import Data.List (groupBy)
import Data.Map (assocs, lookup, mapKeys)
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec.Char (newline)

-- | A rectangular grid of @t@.
data Grid t = Grid
  { _gridHeight :: Int,
    _gridWidth :: Int,
    _gridMap :: Map (Int, Int) t
  }
  deriving (Eq, Show)

$(makeLenses ''Grid)

showGrid :: Show a => Grid a -> Text
showGrid g = unlines $ toText <$> ls
  where
    cells = sortOn (snd . fst) $ sortOn (fst . fst) $ assocs $ g ^. gridMap
    rows = groupBy (\((_, y), _) ((_, y'), _) -> y == y') cells
    ls = fmap (concatMap (\(_, cell) -> show cell)) rows

-- | Given a cell parser, parse a grid of newline-separated rows of cells.
parseGrid :: Parser t -> Parser (Grid t)
parseGrid cell = do
  rows <- some cell `NonEmpty.sepEndBy1` newline
  let r = head rows
  let _gridHeight = length rows
  let _gridWidth = length r
  unless (and $ (== _gridWidth) . length <$> rows) $ error "parseGrid: rows must be the same length for a rectangular grid"
  let _gridMap = fromList $ concatMap (\(y, xs) -> fmap (\(x, p) -> ((x, y), p)) xs) $ zip [0 ..] $ zip [0 ..] <$> toList rows
  return Grid {..}

-- Rotate a grid clockwise by 90 degrees.
rotate :: Grid t -> Grid t
rotate Grid {..} =
  Grid
    { _gridHeight = _gridWidth,
      _gridWidth = _gridHeight,
      _gridMap = mapKeys (\(x, y) -> (- y + (_gridWidth - 1), x)) _gridMap
    }

-- Reflect a grid across the X axis.
reflect :: Grid t -> Grid t
reflect g = over gridMap (mapKeys (first (width - 1 -))) g
  where
    width = view gridWidth g

-- The elements of the dihedral group of order 8 (i.e. the rotational and
-- reflective symmetries of a grid).
dihedral :: Grid t -> [Grid t]
dihedral t = mconcat [[rotation, reflect rotation] | rotation <- rotations]
  where
    rotations = take 4 $ iterate rotate t

data Sides t = Sides
  { top :: t,
    right :: t,
    bottom :: t,
    left :: t
  }
  deriving (Show)

instance Functor Sides where
  fmap f Sides {..} =
    Sides
      { top = f top,
        right = f right,
        bottom = f bottom,
        left = f left
      }

instance Foldable Sides where
  foldr f z Sides {..} = foldr f z [top, right, bottom, left]

-- The 4 edges of a grid.
edges :: Grid t -> Sides [t]
edges g = lookup' <<$>> Sides {..}
  where
    lookup' = Unsafe.fromJust . (`lookup` view gridMap g)
    width = view gridWidth g
    height = view gridHeight g
    xs = [0 .. width - 1]
    ys = [0 .. height - 1]
    top = [(i, 0) | i <- xs]
    right = [(height - 1, i) | i <- ys]
    bottom = [(i, width - 1) | i <- reverse xs]
    left = [(0, i) | i <- reverse ys]
