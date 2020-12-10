module Advent2020.D7 (run, part1) where

import Advent2020.Internal.D7 (Rule (..), parse)
import Data.Graph (reachable, transposeG, graphFromEdges)
import Data.Map (keys)
import Relude

run :: Text -> ([Rule] -> Either Text Int) -> Either Text Int
run contents runner = do
  rules <- parse contents
  runner rules

part1 :: [Rule] -> Either Text Int
part1 rules = do
  let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges $ (\Rule {..} -> (color, color, keys contains)) <$> rules
  let g' = transposeG graph
  root <- maybeToRight "could not find shiny gold bag" $ vertexFromKey "shiny gold"
  let bagVertices = reachable g' root
  return $ length ((\(color, _, _) -> color) . nodeFromVertex <$> bagVertices) - 1 -- (-1) because "shiny gold" is also a reachable node
