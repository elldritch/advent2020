module Advent2020.Internal.D20
  ( Tile (..),
    tileID,
    tileGrid,
    showTile,
    Pixel (..),
    parse,
    corners,
  )
where

import Advent2020.Internal (Grid (..), gridMap, integralP, parseGrid, parseWithPrettyErrors, showGrid, symbol)
import Control.Lens (makeLenses, over, view, (^.))
import Data.Map (foldrWithKey, mapKeys, mapWithKey)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Show (Show (..))
import Relude hiding (show)
import Relude.Extra.Map
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec (eof, sepEndBy1)
import Text.Megaparsec.Char (char, newline)

data Pixel
  = Black
  | White
  deriving (Eq)

instance Show Pixel where
  show Black = "."
  show White = "#"

type Edge = [Pixel]

type TileID = Integer

data Tile = Tile
  { _tileID :: TileID,
    _tileGrid :: Grid Pixel
  }
  deriving (Show, Eq)

$(makeLenses ''Tile)

parse :: Text -> Either Text [Tile]
parse = parseWithPrettyErrors $ tileP `sepEndBy1` newline <* eof
  where
    tileIDP = symbol "Tile" >> integralP <* symbol ":"
    blackP = Black <$ char '.'
    whiteP = White <$ char '#'
    pixelP = blackP <|> whiteP
    tileP = do
      _tileID <- tileIDP
      _ <- newline
      _tileGrid <- parseGrid pixelP
      return Tile {..}

showTile :: Tile -> Text
showTile = showGrid . view tileGrid

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

-- Puzzle inputs are always 10-by-10 square tiles.
squareSize :: Int
squareSize = 10

-- Rotate a square tile clockwise.
rotate :: Tile -> Tile
rotate = over (tileGrid . gridMap) $ mapKeys (\(x, y) -> (- y + (squareSize - 1), x))

-- Reflect a square tile across the X axis.
reflect :: Tile -> Tile
reflect = over (tileGrid . gridMap) $ mapKeys (first (squareSize - 1 -))

-- The elements of the dihedral group of order 8 (i.e. the rotational and
-- reflective symmetries of a square).
dihedral :: Tile -> [Tile]
dihedral t = mconcat [[rotation, reflect rotation] | rotation <- rotations]
  where
    rotations = take 4 $ iterate rotate t

-- The 4 edges of a square tile.
edges :: Tile -> Sides Edge
edges t = lookup' <<$>> Sides {..}
  where
    lookup' :: (Int, Int) -> Pixel
    lookup' = Unsafe.fromJust . (`lookup` view (tileGrid . gridMap) t)
    is = [0 .. squareSize -1]
    top = [(i, 0) | i <- is]
    right = [(squareSize - 1, i) | i <- is]
    bottom = [(i, squareSize - 1) | i <- reverse is]
    left = [(0, i) | i <- reverse is]

-- Edges align if they are equal, or reverse-equal (because you just need to
-- flip the tile).
align :: Edge -> Edge -> Bool
align a b = a == b || a == reverse b

-- Find the corner tiles (i.e. tiles which have 2 edges that do not match any
-- other tiles).
corners :: [Tile] -> [Tile]
corners tiles = cornerTiles
  where
    tileMap :: Map TileID Tile
    tileMap = fromList $ (\t -> (t ^. tileID, t)) <$> tiles

    tileEdges :: Map TileID (Sides Edge)
    tileEdges = edges <$> tileMap

    tileEdgeMatches :: Map TileID (Sides (Set TileID))
    tileEdgeMatches = mapWithKey tileToMatches tileEdges
      where
        tileToMatches :: TileID -> Sides Edge -> Sides (Set TileID)
        tileToMatches tID = fmap (`edgeMatches` delete tID tileEdges)

        -- Collect a set of tiles that this edge could possibly align with.
        edgeMatches :: Edge -> Map TileID (Sides Edge) -> Set TileID
        edgeMatches edge otherTiles =
          foldrWithKey
            ( \otherTileID otherTileEdges acc ->
                if or $ align edge <$> otherTileEdges
                  then Set.insert otherTileID acc
                  else acc
            )
            mempty
            otherTiles

    cornerTiles :: [Tile]
    cornerTiles = Unsafe.fromJust . (`lookup` tileMap) <$> keys (Map.filter ((== 2) . sum . fmap (fromEnum . null)) tileEdgeMatches)
