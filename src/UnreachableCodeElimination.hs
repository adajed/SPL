module UnreachableCodeElimination where

import Data.Set as Set
import Data.Map as Map

import BasicBlock
import IR
import OptimizationUtils

unreachableCodeElimination :: BBGraph -> BBGraph
unreachableCodeElimination g = g''
    where g' = buildBBGraph (flattenBBGraph g)
          ns = dfs g' 1
          prev' = Map.map (Prelude.filter (`Set.member` ns)) (prev g')
          layout' = Map.filterWithKey (\k -> \a -> Set.member k ns) (layout g')
          layout'' = Map.mapWithKey (\k -> \a -> getNextLayout (layout g') ns k) layout'
          g'' = G { ids = Map.filterWithKey (\k -> \a -> Set.member k ns) (ids g')
                 , next = Map.filterWithKey (\k -> \a -> Set.member k ns) (next g')
                 , prev = Map.filterWithKey (\k -> \a -> Set.member k ns) prev'
                 , layout = layout''
                 , args = args g
                 }

getNextLayout :: Map Int (Maybe Int) -> Set Int -> Int -> Maybe Int
getNextLayout m s i =
    case m ! i of
      Nothing -> Nothing
      Just j -> if Set.member j s
                   then Just j
                   else getNextLayout m s j

dfs :: BBGraph -> Int -> Set Int
dfs g n = dfs' Set.empty n
    where dfs' :: Set Int -> Int -> Set Int
          dfs' acc n =
              if Set.member n acc
                 then acc
                 else Prelude.foldl dfs' (Set.insert n acc) ((next g) ! n)


