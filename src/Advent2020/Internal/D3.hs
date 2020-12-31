module Advent2020.Internal.D3
  ( Square (..),
    parse,
    Slope (..),
    treesPerSlope,
  )
where

import Advent2020.Internal (Grid (..), parseGrid, parseWithPrettyErrors)
import Relude
import Relude.Extra
import Text.Megaparsec.Char (char)

data Square
  = Open
  | Tree
  deriving (Show, Eq)

parse :: Text -> Either Text (Grid Square)
parse = parseWithPrettyErrors $ parseGrid squareP
  where
    openP = Open <$ char '.'
    treeP = Tree <$ char '#'
    squareP = openP <|> treeP

squareAt :: Grid Square -> (Int, Int) -> Either Text Square
squareAt Grid {..} (x, y) = maybeToRight ("squareAt: invalid map square: " <> show (x, y)) $ lookup (x', y) gridMap
  where
    x' = x `mod` width

data Slope = Slope
  { right :: Int,
    down :: Int
  }

slopePath :: Slope -> [(Int, Int)]
slopePath Slope {..} = iterate move (0, 0)
  where
    move (x, y) = (x + right, y + down)

treesPerSlope :: Grid Square -> Slope -> Either Text Int
treesPerSlope g@Grid {..} slope = do
  let path = takeWhile (\(_, y) -> y < height) $ slopePath slope
  squares <- sequence $ squareAt g <$> path
  return $ length $ filter (== Tree) squares
