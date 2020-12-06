module Advent2020.D3 (parse, SledMap (..), MapSquare (..), slopePath, run, part1, part2, treesPerSlope, Slope (..)) where

import Advent2020.Internal (Parser, gather, parseWithPrettyErrors)
import Data.Either.Combinators (mapLeft)
import Relude hiding (some)
import Text.Megaparsec (manyTill, some)
import Text.Megaparsec.Char (char, spaceChar)

data MapSquare = Open | Tree deriving (Show, Eq)

newtype SledMap = SledMap
  { rows :: [[MapSquare]]
  }
  deriving (Show, Eq)

parse :: Text -> Either Text SledMap
parse = parseWithPrettyErrors parser

parser :: Parser SledMap
parser = do
  rows <- some rowParser
  return SledMap {rows}
  where
    openSquareParser :: Parser MapSquare
    openSquareParser = char '.' >> return Open

    treeSquareParser :: Parser MapSquare
    treeSquareParser = char '#' >> return Tree

    squareParser :: Parser MapSquare
    squareParser = openSquareParser <|> treeSquareParser

    rowParser :: Parser [MapSquare]
    rowParser = squareParser `manyTill` spaceChar

squareAt :: SledMap -> (Int, Int) -> Maybe MapSquare
squareAt SledMap {..} (x, y) = do
  row <- rows !!? y
  let l = length row
  let x' = x `mod` l
  row !!? x'

data Slope = Slope
  { right :: Int,
    down :: Int
  }

slopePath :: Slope -> [(Int, Int)]
slopePath Slope {..} = iterate move (0, 0)
  where
    move (x, y) = (x + right, y + down)

run :: Text -> (SledMap -> Either Text Int) -> Either Text Int
run contents runner = do
  smap <- parse contents
  runner smap

part1 :: SledMap -> Either Text Int
part1 = flip treesPerSlope Slope {right = 3, down = 1}

treesPerSlope :: SledMap -> Slope -> Either Text Int
treesPerSlope smap@SledMap {..} slope = do
  let path = takeWhile (\(_, y) -> y < length rows) $ slopePath slope
  squares <- mapLeft mconcat $ gather $ maybeToRight "invalid square" . squareAt smap <$> path
  return $ sum $ fmap countSquare squares
  where
    countSquare = \case
      Open -> 0
      Tree -> 1

part2 :: SledMap -> Either Text Int
part2 smap = do
  trees <- mapLeft mconcat $ gather $ treesPerSlope smap <$> slopes
  return $ product trees
  where
    slopes =
      [ Slope {right = 1, down = 1},
        Slope {right = 3, down = 1},
        Slope {right = 5, down = 1},
        Slope {right = 7, down = 1},
        Slope {right = 1, down = 2}
      ]
