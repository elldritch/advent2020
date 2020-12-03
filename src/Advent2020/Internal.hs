module Advent2020.Internal (gather) where

import Relude

gather :: [Either e a] -> Either [e] [a]
gather = foldr foldEither (Right [])
  where
    foldEither :: Either e a -> Either [e] [a] -> Either [e] [a]
    foldEither e acc = case acc of
      Right rs -> case e of
        Right r -> Right (r : rs)
        Left l -> Left [l]
      Left ls -> case e of
        Right _ -> Left ls
        Left l -> Left (l : ls)
