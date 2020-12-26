module Advent2020.Internal.D17
  ( Pocket (..),
    numCubes,
    Position,
    parse,
    step,
    stepN,
    Hyperposition,
    parse',
    step',
    stepN',
  )
where

import Advent2020.Internal (parseWithPrettyErrors, unsafeNonEmpty)
import qualified Data.List as List
import Relude
import Relude.Extra.Map
import Text.Megaparsec (eof, someTill)
import Text.Megaparsec.Char (char, newline)

type Position = (Int, Int, Int)

type Hyperposition = (Int, Int, Int, Int)

newtype Pocket t = Pocket {activeCubes :: Set t}
  deriving (Show, Eq)

numCubes :: (Ord t) => Pocket t -> Int
numCubes (Pocket actives) = size actives

parse_ :: (Ord t) => ((Int, Int) -> t) -> Text -> Either Text (Pocket t)
parse_ interpretPosition = parseWithPrettyErrors $ do
  rows <- row `someTill` eof
  let cubes = concatMap (\(y, xs) -> fmap (\(x, p) -> ((x, y), p)) xs) $ zip [0 ..] $ zip [0 ..] <$> rows
  let cubes' = interpretPosition . fst <$> filter snd cubes
  return $ Pocket $ fromList cubes'
  where
    activeP = char '#' >> return True
    inactiveP = char '.' >> return False
    row = (activeP <|> inactiveP) `someTill` newline

parse :: Text -> Either Text (Pocket Position)
parse = parse_ $ \(x, y) -> (x, y, 0)

parse' :: Text -> Either Text (Pocket Hyperposition)
parse' = parse_ $ \(x, y) -> (x, y, 0, 0)

step_ :: (Ord t) => (t -> [t]) -> Pocket t -> Pocket t
step_ neighbors p@(Pocket actives) = Pocket $ fromList nextActives
  where
    actives' = toList actives
    -- Cubes only consider their neighbors, so iterate everything that's active
    -- plus everything that's a neighbor of something active.
    relevant = actives' ++ mconcat (neighbors <$> actives')
    next = zip relevant $ stepPosition neighbors p <$> relevant
    nextActives = fst <$> filter snd next

stepPosition :: (Ord t) => (t -> [t]) -> Pocket t -> t -> Bool
stepPosition neighbors (Pocket actives) position
  | isActive = activeNeighbors == 2 || activeNeighbors == 3
  | otherwise = activeNeighbors == 3
  where
    isActive = position `member` actives
    activeNeighbors = length $ filter (`member` actives) $ neighbors position

neighborsOf :: Position -> [Position]
neighborsOf (x, y, z) =
  List.delete
    (x, y, z)
    [ (x + dx, y + dy, z + dz)
      | dx <- [-1, 0, 1],
        dy <- [-1, 0, 1],
        dz <- [-1, 0, 1]
    ]

hyperNeighborsOf :: Hyperposition -> [Hyperposition]
hyperNeighborsOf (x, y, z, w) =
  List.delete
    (x, y, z, w)
    [ (x + dx, y + dy, z + dz, w + dw)
      | dx <- [-1, 0, 1],
        dy <- [-1, 0, 1],
        dz <- [-1, 0, 1],
        dw <- [-1, 0, 1]
    ]

step :: Pocket Position -> Pocket Position
step = step_ neighborsOf

step' :: Pocket Hyperposition -> Pocket Hyperposition
step' = step_ hyperNeighborsOf

stepN_ :: (Pocket t -> Pocket t) -> Int -> Pocket t -> Pocket t
stepN_ stepper n = head . unsafeNonEmpty . drop n . iterate stepper

stepN :: Int -> Pocket Position -> Pocket Position
stepN = stepN_ step

stepN' :: Int -> Pocket Hyperposition -> Pocket Hyperposition
stepN' = stepN_ step'
