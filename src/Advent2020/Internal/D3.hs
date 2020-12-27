module Advent2020.Internal.D3
  ( MapSquare (..),
    SledMap (..),
    parse,
    squareAt,
    Slope (..),
    treesPerSlope,
  )
where

import Advent2020.Internal (Parser, parseWithPrettyErrors)
import Relude
import Text.Megaparsec (eof, hidden, manyTill, someTill)
import Text.Megaparsec.Char (char, newline)

data MapSquare = Open | Tree deriving (Show, Eq)

newtype SledMap = SledMap
  { rows :: [[MapSquare]]
  }
  deriving (Show, Eq)

parse :: Text -> Either Text SledMap
parse = parseWithPrettyErrors parser

parser :: Parser SledMap
parser = do
  rows <- rowParser `someTill` hidden eof
  return SledMap {rows}
  where
    openSquareParser :: Parser MapSquare
    openSquareParser = char '.' >> return Open

    treeSquareParser :: Parser MapSquare
    treeSquareParser = char '#' >> return Tree

    squareParser :: Parser MapSquare
    squareParser = openSquareParser <|> treeSquareParser

    rowParser :: Parser [MapSquare]
    rowParser = squareParser `manyTill` newline

squareAt :: SledMap -> (Int, Int) -> Either Text MapSquare
squareAt SledMap {..} (x, y) = do
  row <- maybeToRight (err "y") $ rows !!? y
  let l = length row
  let x' = x `mod` l
  maybeToRight (err "x") $ row !!? x'
  where
    err axis = "invalid map square (" <> axis <> " out-of-bounds): " <> show (x, y)

data Slope = Slope
  { right :: Int,
    down :: Int
  }

slopePath :: Slope -> [(Int, Int)]
slopePath Slope {..} = iterate move (0, 0)
  where
    move (x, y) = (x + right, y + down)

treesPerSlope :: SledMap -> Slope -> Either Text Int
treesPerSlope smap@SledMap {..} slope = do
  let path = takeWhile (\(_, y) -> y < length rows) $ slopePath slope
  squares <- sequence $ squareAt smap <$> path
  return $ length $ filter (== Tree) squares
