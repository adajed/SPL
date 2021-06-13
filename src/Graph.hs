module Graph where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)


type Vertex = Int
type Edge = (Vertex, Vertex)

data Graph a = Graph { nodes :: [a]
                     , next :: M.Map Vertex [Vertex]
                     , prev :: M.Map Vertex [Vertex]
                     }

buildGraphFromEdges :: Ord a => [(a, a)] -> (Graph a, M.Map a Vertex)
buildGraphFromEdges edges = (Graph { nodes = nodes', next = next', prev = prev' }, mapping)
    where nodes' = S.toList (S.union (S.fromList (map fst edges)) (S.fromList (map snd edges)))
          mapping = M.fromList (zip nodes' [1..])
          edges' = map (\(x, y) -> (mapping M.! x, mapping M.! y)) edges
          size' = M.size mapping
          next' = M.fromList (map (\i -> (i, getNext_ edges' i)) (M.elems mapping))
          prev' = M.fromList (map (\i -> (i, getPrev_ edges' i)) (M.elems mapping))

-- given list of edges and a vertex 'v'
-- returns a list of vertices that are reachable from 'v' in one step
getNext_ :: [Edge] -> Vertex -> [Vertex]
getNext_ edges v = nub (map snd (filter ((v==) . fst) edges))

-- given list of edges and a vertex 'v'
-- returns a list of vertices that can reach 'v' in one step
getPrev_ :: [Edge] -> Vertex -> [Vertex]
getPrev_ edges v = nub (map fst (filter ((v==) . snd) edges))


-- removes edge (if it exists) from the graph
removeEdge :: Edge -> Graph a -> Graph a
removeEdge (v_src, v_dest) graph = graph { next = next', prev = prev' }
    where next' = M.adjust (filter (not . (v_dest==))) v_src (next graph)
          prev' = M.adjust (filter (not . (v_src==))) v_dest (prev graph)


-- removes vertex from the graph along with all its edges
-- removeVertex :: Vertex -> Graph -> Graph
-- removeVertex v graph =
