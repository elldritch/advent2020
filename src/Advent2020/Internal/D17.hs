module Advent2020.Internal.D17 (Pocket (..), parse, step) where

import Advent2020.Internal (parseWithPrettyErrors)
import qualified Data.List as List
import Relude
import Relude.Extra.Map
import Text.Megaparsec (eof, someTill)
import Text.Megaparsec.Char (char, newline)

type Position = (Int, Int, Int)

newtype Pocket = Pocket {activeCubes :: Set Position}
  deriving (Show, Eq, Ord)

parse :: Text -> Either Text Pocket
parse = parseWithPrettyErrors $ do
  rows <- row `someTill` eof
  let cubes = concatMap (\(y, xs) -> fmap (\(x, p) -> ((x, y), p)) xs) $ zip [0 ..] $ zip [0 ..] <$> rows
  let cubes' = (\(x, y) -> (x, y, 0)) . fst <$> filter snd cubes
  return $ Pocket $ fromList cubes'
  where
    activeP = char '#' >> return True
    inactiveP = char '.' >> return False
    row = (activeP <|> inactiveP) `someTill` newline

neighborsOf :: Position -> [Position]
neighborsOf (x, y, z) =
  List.delete
    (x, y, z)
    [ (x + dx, y + dy, z + dz)
      | dx <- [-1, 0, 1],
        dy <- [-1, 0, 1],
        dz <- [-1, 0, 1]
    ]

{-# HLINT ignore step' "Use guards" #-}
step :: Pocket -> Pocket
step p@(Pocket actives) = Pocket $ fromList nextActives
  where
    actives' = toList actives
    -- Cubes only consider their neighbors, so iterate everything that's active
    -- plus everything that's a neighbor of something active.
    relevant = actives' ++ mconcat (neighborsOf <$> actives')
    next = zip relevant $ step' p <$> relevant
    nextActives = fst <$> filter snd next

step' :: Pocket -> Position -> Bool
step' (Pocket actives) position
  | isActive = activeNeighbors == 2 || activeNeighbors == 3
  | otherwise = activeNeighbors == 3
  where
    isActive = position `member` actives
    activeNeighbors = length $ filter (`member` actives) $ neighborsOf position
