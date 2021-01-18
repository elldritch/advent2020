module Advent2020.Internal.D23
  ( Cups,
    makeCups,
    parse,
    step,
  )
where

import Advent2020.Internal (parseWithPrettyErrors)
import Data.Char (digitToInt)
import Data.CircularList (CList, focus, rightElements, rotL, rotR, rotateTo)
import qualified Data.CircularList as CL
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (digitChar, newline)

-- | A circle of cups.
--
-- The "current" cup is the focus of the CircularList. Clockwise is rightwards.
type Cups = CList Int

makeCups :: [Int] -> Cups
makeCups = CL.fromList

parse :: Text -> Either Text Cups
parse = parseWithPrettyErrors $ do
  ns <- digitToInt <<$>> some digitChar
  _ <- newline
  eof
  return $ makeCups ns

-- | Make a move.
step :: Cups -> Cups
step cups = rotR atCurrent
  where
    -- Invariant: the current cup is the focus.
    current :: Int
    current = Unsafe.fromJust $ focus cups

    -- Cups clockwise of the current cup.
    clockwiseOfCurrent :: [Int]
    clockwiseOfCurrent = rightElements $ rotR cups

    -- Pick up 3 cups.
    pickedUp :: [Int]
    pickedUp = take 3 clockwiseOfCurrent

    -- Remove picked up cups.
    remaining :: Cups
    remaining = rotL $ makeCups $ drop 3 clockwiseOfCurrent

    -- Largest cup label.
    highestLabel :: Int
    highestLabel = foldr max 0 cups

    -- Pick destination label by subtracting 1 from current cup, until you find
    -- a cup that hasn't been picked up yet. Wrap around if you go below the
    -- lowest possible value.
    destination :: Int
    destination = Unsafe.fromJust $ find (`elem` remaining) $ iterate (\x -> let x' = x - 1 in if x' < 0 then highestLabel else x') (current - 1)

    -- Focus at destination.
    atDestination :: Cups
    atDestination = Unsafe.fromJust $ rotateTo destination remaining

    -- Insert to the right of the destination.
    inserted :: Cups
    inserted = makeCups $ destination : pickedUp ++ drop 1 (rightElements atDestination)

    -- Rotate back to the starting current cup.
    atCurrent :: Cups
    atCurrent = Unsafe.fromJust $ rotateTo current inserted
