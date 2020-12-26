module Advent2020.D10 (run, part1, part2) where

import Advent2020.Internal (largest, runNumbers)
import Advent2020.Internal.D10 (joltageDifferences)
import Algebra.Graph.Acyclic.AdjacencyMap (AdjacencyMap, preSet, toAcyclic, topSort)
import Algebra.Graph.AdjacencyMap (edges)
import Relude
import Relude.Extra.Map

run :: (NonEmpty Int -> Either Text Int) -> Text -> Either Text Int
run = runNumbers

part1 :: NonEmpty Int -> Either Text Int
part1 ns = let (a, _, c) = joltageDifferences (toList ns) in return $ a * c

type Vertex = Int

part2 :: NonEmpty Int -> Either Text Int
part2 ns = do
  let laptop = 3 + largest ns
  graph <- maybeToRight "impossible: adapter graph is cyclic" makeGraph
  paths <- pathsTo graph
  maybeToRight "could not find count for laptop vertex" $ lookup laptop paths
  where
    makeGraph :: Maybe (AdjacencyMap Vertex)
    makeGraph = toAcyclic $ edges adapterEdges

    adapterEdges :: [(Vertex, Vertex)]
    adapterEdges = tailToEdges `concatMap` tails (sort $ toList ns ++ [0, 3 + largest ns])

    tailToEdges :: [Vertex] -> [(Vertex, Vertex)]
    tailToEdges (t : ts) = (t,) <$> takeWhile (\x -> x - t <= 3) ts
    tailToEdges [] = []

    pathsTo :: AdjacencyMap Vertex -> Either Text (Map Vertex Int)
    pathsTo graph = foldlM (countPaths graph) mempty $ topSort graph

    countPaths :: AdjacencyMap Vertex -> Map Vertex Int -> Vertex -> Either Text (Map Vertex Int)
    countPaths _ _ 0 = return $ one (0, 1)
    countPaths graph pathsCounts vertex = do
      counts <- sequence $ maybeToRight "could not find count for pre-vertex" . (`lookup` pathsCounts) <$> toList from
      return $ insert vertex (sum counts) pathsCounts
      where
        from :: Set Vertex
        from = preSet vertex graph
