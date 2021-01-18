module Advent2020.D23 (run, part1) where

import Advent2020.Internal (simpleRun, unsafeNonEmpty)
import Advent2020.Internal.D23 (Cups, parse, step)
import Data.CircularList (rightElements, rotateTo)
import Relude

run :: (Cups -> Either Text Text) -> Text -> Either Text Text
run = simpleRun parse

part1 :: Cups -> Either Text Text
part1 cups = do
  at1 <- maybeToRight "no cup labelled 1" $ rotateTo 1 cups'
  return $ mconcat $ show <$> drop 1 (rightElements at1)
  where
    cups' = head $ unsafeNonEmpty $ drop 100 $ iterate step cups
