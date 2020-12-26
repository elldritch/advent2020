module Advent2020.Internal.Function (fixed) where

import Relude

-- | Find the fixed point of a function given an initial argument.
fixed :: (Eq a) => (a -> a) -> a -> a
fixed f a
  | a == a' = a
  | otherwise = fixed f a'
  where
    a' = f a
