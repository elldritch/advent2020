module Advent2020.D20 (run, part1) where

import Advent2020.Internal (simpleRun')
import Advent2020.Internal.D20 (Tile, TileID, corners, parse, tileID)
import Relude
import Relude.Extra.Lens

run :: (Map TileID Tile -> Integer) -> Text -> Either Text Integer
run = simpleRun' parse return

part1 :: Map TileID Tile -> Integer
part1 tiles = product $ (^. tileID) <$> corners tiles
