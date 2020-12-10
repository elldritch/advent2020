module Advent2020.D8 (part1) where

import Advent2020.Internal.D8 (Machine (..), parse, step)
import Data.Set (insert, member)
import Relude

part1 :: Text -> Either Text Int
part1 contents = do
  program <- parse contents
  let machines = iterate (>>= step program) $ Right $ Machine {accumulator = 0, programCounter = 0}
  takeUntilLoop mempty machines
  where
    takeUntilLoop :: Set Int -> [Either Text Machine] -> Either Text Int
    takeUntilLoop seen (m : ms) = do
      Machine {..} <- m
      if member programCounter seen
        then return accumulator
        else takeUntilLoop (insert programCounter seen) ms
    takeUntilLoop _ [] = Left "program never loops"
