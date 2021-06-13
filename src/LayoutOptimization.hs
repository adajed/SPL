module LayoutOptimization where

import qualified Data.Map as M

import BasicBlock
import IR

layoutOptimization :: BBGraph -> BBGraph
layoutOptimization graph =
    case findBlocksToMerge graph of
      Nothing -> graph
      Just (i, j) -> let newGraph = mergeNodes i j graph
                      in layoutOptimization newGraph

findBlocksToMerge :: BBGraph -> Maybe (Int, Int)
findBlocksToMerge graph = help nodesWithSingleNext
    where nodesWithSingleNext = filter ((==1) . length . ((next graph) M.!)) (layout graph)
          hasSinglePrev :: Int -> Bool
          hasSinglePrev i = length ((prev graph) M.! i) == 1
          help :: [Int] -> Maybe (Int, Int)
          help [] = Nothing
          help (i:is) = let j = head ((next graph) M.! i)
                         in if hasSinglePrev j then Just (i, j) else help is

mergeNodes :: Int -> Int -> BBGraph -> BBGraph
mergeNodes idxFirst idxSecond graph = graph { ids = ids', next = next', prev = prev', layout = layout', start = start', end = end'}
    where nodes = ids graph
          newBB = BB { bbLabel = bbLabel (nodes M.! idxFirst), bbCode = (init (bbCode (nodes M.! idxFirst))) ++ (bbCode (nodes M.! idxSecond)) }
          ids' = M.insert idxSecond newBB $ M.delete idxFirst (ids graph)
          f i = if i == idxFirst then idxSecond else i
          next' = M.map (map f) $ M.delete idxFirst (next graph)
          prev' = M.insert idxSecond ((prev graph) M.! idxFirst) $ M.delete idxFirst (prev graph)
          start' = if (start graph) == idxFirst then idxSecond else start graph
          end' = map f (end graph)
          layout' = filter (not . (==idxFirst)) (layout graph)

