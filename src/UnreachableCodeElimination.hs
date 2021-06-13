module UnreachableCodeElimination where

import qualified Data.Map as M
import qualified Data.Set as S

import AbsSPL
import BasicBlock
import IR
import OptimizationUtils
import Token


unreachableCodeElimination :: VIdent -> BBGraph -> BBGraph
unreachableCodeElimination fName g = g'
    where reachable = dfs g (start g)
          pred v = S.member v reachable
          g' = G { ids = M.filterWithKey (\k _ -> pred k) (ids g)
                  , next = filterMap pred (next g)
                  , prev = filterMap pred (prev g)
                  , layout = filter pred (layout g)
                  , args = args g
                  , start = start g
                  , end = filter pred (end g)
                  }
    -- where g' = buildBBGraph fName (map ((ids g) M.!) (layout g))


filterMap :: (Int -> Bool) -> M.Map Int [Int] -> M.Map Int [Int]
filterMap p m = M.filterWithKey (\k _ -> p k) $ M.map (filter p) m


dfs :: BBGraph -> Int -> S.Set Int
dfs g n = dfs' S.empty n
    where dfs' :: S.Set Int -> Int -> S.Set Int
          dfs' acc n =
              if S.member n acc
                 then acc
                 else foldl dfs' (S.insert n acc) ((next g) M.! n)
