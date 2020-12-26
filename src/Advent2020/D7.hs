module Advent2020.D7 (run, part1, part2) where

import Advent2020.Internal (simpleRun)
import Advent2020.Internal.D7 (Rule (..), parse)
import Data.Graph (graphFromEdges, reachable, transposeG)
import Data.Map (foldrWithKey)
import Relude
import Relude.Extra.Map

run :: ([Rule] -> Either Text Int) -> Text -> Either Text Int
run = simpleRun parse

part1 :: [Rule] -> Either Text Int
part1 rules = do
  let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges $ (\Rule {..} -> (color, color, keys contains)) <$> rules
  let g' = transposeG graph
  root <- maybeToRight "could not find shiny gold bag" $ vertexFromKey "shiny gold"
  let bagVertices = reachable g' root
  return $ length ((\(color, _, _) -> color) . nodeFromVertex <$> bagVertices) - 1 -- (-1) because "shiny gold" is also a reachable node

type RuleMap = Map Text (Map Text Int)

part2 :: [Rule] -> Either Text Int
part2 rules = do
  count <- bagContains m "shiny gold"
  return $ count - 1
  where
    m = fromList $ (\Rule {..} -> (color, contains)) <$> rules

bagContains :: RuleMap -> Text -> Either Text Int
bagContains m color = do
  contains <- maybeToRight ("invalid rule map: does not contain color " <> show color) $ lookup color m
  if null contains
    then return 1
    else foldrWithKey f (Right 1) contains
  where
    f containedColor containedCount bagCount = do
      prev <- bagCount
      contained <- bagContains m containedColor
      return $ prev + containedCount * contained
