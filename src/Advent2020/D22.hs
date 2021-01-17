module Advent2020.D22 (run, part1, part2) where

import Advent2020.Internal (simpleRun')
import Advent2020.Internal.D22 (Deck, Game, Player, parse, play, play', score)
import Relude

run :: (Game -> (Player, Deck)) -> Text -> Either Text Int
run = simpleRun' parse (return . score . snd)

part1 :: Game -> (Player, Deck)
part1 = play

part2 :: Game -> (Player, Deck)
part2 = play'
