module Advent2020.Internal.Grid
  ( Grid (..),
    gridHeight,
    gridWidth,
    gridMap,
    parseGrid,
    showGrid,
  )
where

import Advent2020.Internal.Parse (Parser)
import Control.Lens (makeLenses, (^.))
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import Data.List (groupBy)
import Data.Map (assocs)
import Relude
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
