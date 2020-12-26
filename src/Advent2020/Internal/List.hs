module Advent2020.Internal.List
  ( setAt,
    pairs,
    windows,
    smallest,
    largest,
    unsafeNonEmpty,
  )
where

import Relude
import qualified Relude.Unsafe as Unsafe

-- | Set a value at an index in a list.
setAt :: Int -> a -> [a] -> Either Text [a]
setAt 0 v (_ : xs) = return $ v : xs
setAt n v (x : xs) = setAt (n - 1) v xs <&> (x :)
setAt _ _ [] = fail "setAt: index out-of-bounds"

-- | Pairs of a list.
pairs :: [a] -> [(a, a)]
pairs (x : xs) = ((x,) <$> xs) ++ pairs xs
pairs [] = []

-- | Fixed-size sliding windows of a list.
windows :: Int -> [a] -> [[a]]
windows n xs = filter (\l -> length l == n) $ take n <$> tails xs

-- | Returns the smallest element of the list.
smallest :: (Ord a) => NonEmpty a -> a
smallest xs = foldr min (head xs) xs

-- | Returns the largest element of the list.
largest :: (Ord a) => NonEmpty a -> a
largest xs = foldr max (head xs) xs

-- | Transform a list into its nonempty equivalent. Only use when safe by construction.
unsafeNonEmpty :: [a] -> NonEmpty a
unsafeNonEmpty = Unsafe.fromJust . nonEmpty
