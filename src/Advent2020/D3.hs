module Advent2020.D3 (parse, SledMap (..), MapSquare (..), slopePath, run, part1) where

import Relude hiding (some)
import Text.Megaparsec (runParser, errorBundlePretty, some, manyTill, Parsec)
import Text.Megaparsec.Char (spaceChar, char)
import Advent2020.Internal (gather)
import Data.Either.Combinators (mapLeft)

data MapSquare = Open | Tree deriving (Show, Eq)

newtype SledMap = SledMap
  { rows :: [[MapSquare]]
  }
  deriving (Show, Eq)

parse :: Text -> Either Text SledMap
parse contents = case runParser parser "" contents of
  Right smap -> Right smap
  Left errs -> Left $ toText $ errorBundlePretty errs

type Parser = Parsec Void Text

parser :: Parser SledMap
parser = do
  rows <- some rowParser
  return SledMap{rows}
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
squareAt SledMap{..} (x, y) = do
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
part1 smap@SledMap{..} = do
  let path = take (length rows) $ slopePath Slope{right=3, down=1}
  squares <- mapLeft mconcat $ gather $ maybeToRight "invalid square" . squareAt smap <$> path
  return $ sum $ fmap countSquare squares
  where
    countSquare = \case
      Open -> 0
      Tree -> 1
