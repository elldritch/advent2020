module Advent2020.Internal.D3
  ( Square (..),
    parse,
    Slope (..),
    treesPerSlope,
  )
where

import Advent2020.Internal (Grid (..), Position, gridHeight, gridMap, gridWidth, parseGrid, parseWithPrettyErrors)
import GHC.Show (Show (..))
import Relude hiding (show)
import Relude.Extra.Lens
import Relude.Extra.Map
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (char)

data Square
  = Open
  | Tree
  deriving (Eq)

instance Show Square where
  show Open = "."
  show Tree = "#"

parse :: Text -> Either Text (Grid Square)
parse = parseWithPrettyErrors $ parseGrid squareP <* eof
  where
    openP = Open <$ char '.'
    treeP = Tree <$ char '#'
    squareP = openP <|> treeP

squareAt :: Grid Square -> Position -> Either Text Square
squareAt g (x, y) = maybeToRight ("squareAt: invalid map square: " <> toText (show (x, y))) $ lookup (x', y) $ g ^. gridMap
  where
    x' = x `mod` g ^. gridWidth

data Slope = Slope
  { right :: Int,
    down :: Int
  }

slopePath :: Slope -> [Position]
slopePath Slope {..} = iterate move (0, 0)
  where
    move (x, y) = (x + right, y + down)

treesPerSlope :: Grid Square -> Slope -> Either Text Int
treesPerSlope g slope = do
  let path = takeWhile (\(_, y) -> y < g ^. gridHeight) $ slopePath slope
  squares <- sequence $ squareAt g <$> path
  return $ length $ filter (== Tree) squares
