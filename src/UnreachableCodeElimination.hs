module UnreachableCodeElimination where

import qualified Data.Map as M
import qualified Data.Set as S

import AbsSPL
import BasicBlock
import IR
import OptimizationUtils

unreachableCodeElimination :: VIdent -> BBGraph -> BBGraph
unreachableCodeElimination fName g = g''
    where g' = buildBBGraph fName (map ((ids g) M.!) (flattenBBGraph g))
          ns = dfs g' 1
          prev' = M.map (filter (`S.member` ns)) (prev g')
          layout' = M.filterWithKey (\k -> \a -> S.member k ns) (layout g')
          layout'' = M.mapWithKey (\k -> \a -> getNextLayout (layout g') ns k) layout'
          g'' = G { ids = M.filterWithKey (\k -> \a -> S.member k ns) (ids g')
                 , next = M.filterWithKey (\k -> \a -> S.member k ns) (next g')
                 , prev = M.filterWithKey (\k -> \a -> S.member k ns) prev'
                 , layout = layout''
                 , args = args g
                 }

getNextLayout :: M.Map Int (Maybe Int) -> S.Set Int -> Int -> Maybe Int
getNextLayout m s i =
    case m M.! i of
      Nothing -> Nothing
      Just j -> if S.member j s
                   then Just j
                   else getNextLayout m s j

dfs :: BBGraph -> Int -> S.Set Int
dfs g n = dfs' S.empty n
    where dfs' :: S.Set Int -> Int -> S.Set Int
          dfs' acc n =
              if S.member n acc
                 then acc
                 else foldl dfs' (S.insert n acc) ((next g) M.! n)


