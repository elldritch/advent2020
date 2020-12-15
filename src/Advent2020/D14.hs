module Advent2020.D14 (run, part1) where

import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D14 (Machine (..), Program, execute, parse)
import Relude

run :: (Program -> Either Text Int) -> Text -> Either Text Int
run = simpleRun parse

part1 :: Program -> Either Text Int
part1 p = do
  let Machine {..} = execute p
  return $ fromInteger $ sum memory
