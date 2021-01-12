module Advent2020.Internal.D20
  ( Tile (..),
    TileID (..),
    tileID,
    tileGrid,
    showTile,
    Pixel (..),
    parse,
    corners,
  )
where

import Advent2020.Internal (Grid (..), Sides (..), edges, integralP, parseGrid, parseWithPrettyErrors, showGrid, symbol, unsafeNonEmpty)
import Control.Lens (makeLenses, view)
import Data.Map (foldrWithKey, mapWithKey)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tree (Tree, unfoldForestM)
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

newtype TileID = TileID {unTileID :: Integer}
  deriving (Show, Ord, Eq)

data Tile = Tile
  { _tileID :: TileID,
    _tileGrid :: Grid Pixel
  }
  deriving (Show, Eq)

$(makeLenses ''Tile)

parse :: Text -> Either Text (Map TileID Tile)
parse = parseWithPrettyErrors $ fromList <$> ((\t@Tile {..} -> (_tileID, t)) <<$>> tilesP)
  where
    tileIDP = symbol "Tile" >> TileID <$> integralP <* symbol ":"
    blackP = Black <$ char '.'
    whiteP = White <$ char '#'
    pixelP = blackP <|> whiteP
    tileP = do
      _tileID <- tileIDP
      _ <- newline
      _tileGrid <- parseGrid pixelP
      return Tile {..}
    tilesP = tileP `sepEndBy1` newline <* eof

showTile :: Tile -> Text
showTile = showGrid . view tileGrid

-- Edges align if they are equal, or reverse-equal (because you just need to
-- flip the tile).
align :: Edge -> Edge -> Bool
align a b = a == b || a == reverse b

-- For each tile, for each side of the tile, get the set of other tiles with
-- matching borders.
matchingBorders :: Map TileID Tile -> Map TileID (Sides (Set TileID))
matchingBorders tiles = mapWithKey tileToMatches tileEdges
  where
    tileEdges :: Map TileID (Sides Edge)
    tileEdges = edges . view tileGrid <$> tiles

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

-- Find the corner tiles (i.e. tiles which have 2 edges that do not match any
-- other tiles).
corners :: Map TileID Tile -> [Tile]
corners tiles = Unsafe.fromJust . (`lookup` tiles) <$> keys (Map.filter ((== 2) . sum . fmap (fromEnum . null)) (matchingBorders tiles))

-- 1. Start the top-left (i.e. the (0, 0)) a corner. WLOG, assume it's the correct orientation.
-- 2. For each edge of the current tile, add the matching tile, making sure it's in the right
-- orientation.
-- 3. DFS until the whole picture is complete.
reconstruct :: Map TileID Tile -> Grid Tile
reconstruct tiles = toGrid $ execState unfoldGrid $ one ((0, 0), topLeftCornerTile)
  where
    lookup' :: TileID -> Tile
    lookup' = Unsafe.fromJust . (`lookup` tiles)

    matches :: Map TileID (Sides (Set TileID))
    matches = matchingBorders tiles

    topLeftCornerTile :: Tile
    topLeftCornerTile = head $ unsafeNonEmpty $ fmap lookup' $ keys $ Map.filter (\sides -> null (top sides) && null (left sides)) matches

    toGrid :: Map (Int, Int) Tile -> Grid Tile
    toGrid tileGridMap =
      Grid
        { _gridMap = tileGridMap,
          _gridHeight = undefined,
          _gridWidth = undefined
        }

    returningUnit :: (Monad m) => m a -> m ((), a)
    returningUnit m = ((),) <$> m

    unfoldGrid :: State (Map (Int, Int) Tile) [Tree ()]
    unfoldGrid = unfoldForestM (returningUnit . f) [(0, 1), (1, 0)]

    f :: (Int, Int) -> State (Map (Int, Int) Tile) [(Int, Int)]
    f (x, y) = do
      -- Get neighbors
      -- For each neighbor:
      -- If tile set in state map, ignore (assume correct -- maybe validate?)
      -- Else, try to set tile in state map to tile's border match (if no matches exist, it's either an edge or corner)
      -- Return list of coordinates of new matches that were not previously set to continue unfolding
      undefined
