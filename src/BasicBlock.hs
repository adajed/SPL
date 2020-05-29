module BasicBlock where

import Data.Map as Map

import AbsSPL
import IR

data BasicBlock = BB VIdent [IR]
    deriving (Eq)

data BBGraph = G { ids :: Map Int BasicBlock
                 , next :: Map Int [Int]
                 , prev :: Map Int [Int]
                 , layout :: Map Int (Maybe Int)
                 , args :: [Int] }
    deriving (Eq)

splitIntoBasicBlocks :: [IR] -> BBGraph
splitIntoBasicBlocks ir = buildBBGraph (h [] 0 ir)
    where h bbs _ [] = reverse bbs
          h bbs n xs = h ((BB name bb):bbs) n' xs'
            where (name, bb, xs', n') = getBasicBlock xs n


getBasicBlock :: [IR] -> Int -> (VIdent, [IR], [IR], Int)
getBasicBlock ((IR_Label name):xs) n = (name, reverse bb, xs', n)
    where (bb, xs') = getBasicBlock' xs []
getBasicBlock xs n = (VIdent (".temp" ++ show n), reverse bb, xs', n + 1)
    where (bb, xs') = getBasicBlock' xs []

getBasicBlock' :: [IR] -> [IR] -> ([IR], [IR])
getBasicBlock' [] ys = (ys, [])
getBasicBlock' (x@(IR_Jump _):xs) ys = ((x:ys), xs)
getBasicBlock' (x@(IR_CondJump _ _ _ _):xs) ys = ((x:ys), xs)
getBasicBlock' (x@(IR_Return _):xs) ys = ((x:ys), xs)
getBasicBlock' (x@(IR_Label _):xs) ys = (ys, x:xs)
getBasicBlock' (ir:xs) ys = getBasicBlock' xs (ir:ys)

buildBBGraph :: [BasicBlock] -> BBGraph
buildBBGraph bbs = G { ids = ids', next = next', prev = prev', layout = layout', args = args' }
    where bbs' = [BB (VIdent "__START__") []] ++ bbs ++ [BB (VIdent "__END__") []]
          ids' = Map.fromList (zip [1..] bbs')
          n i (BB name _) = if name == VIdent "__END__" then Nothing else Just (i + 1)
          layout' = Map.mapWithKey n ids'
          ids_rev = Map.fromList (zip (Prelude.map (\(BB name _) -> name) bbs') [1..])
          next' = Map.fromList (zip [1..] (Prelude.map (getNext ids_rev) bbs'))
          prev' = Map.fromList (zip [1..] (Prelude.map (getPrev ids_rev next') bbs'))
          takeArg ir = case ir of { IR_Argument (SVar _ s) -> [s] ; _ -> [] }
          args' = concat (Prelude.map (\(BB _ xs) -> concat (Prelude.map takeArg xs)) bbs)

getNext :: Map VIdent Int -> BasicBlock -> [Int]
getNext ids (BB name []) =
    let n = ids ! name
     in if n < (size ids) then [n+1] else []
getNext ids (BB name xs) =
    let n = ids ! name
        next = if n < size ids then [n+1] else []
     in case last xs of
          IR_Jump label -> [ids ! label]
          IR_CondJump _ _ _ label -> (ids ! label):next
          IR_Return _ -> [size ids]
          _ -> next

getPrev :: Map VIdent Int -> Map Int [Int] -> BasicBlock -> [Int]
getPrev ids next (BB name _) =
    let n = ids ! name
        next' = Map.toList next
     in Prelude.map fst (Prelude.filter ((elem n) . snd) next')

layoutBBGraph :: BBGraph -> ([IR], [Int])
layoutBBGraph g = (middle (concat (Prelude.map h (l 1 []))), args g)
    where h i = f ((ids g) ! i)
          f (BB name xs) = (IR_Label name):xs
          l i acc = case ((layout g) ! i) of
                      Just j -> l j (i:acc)
                      Nothing -> reverse (i:acc)
          middle xs = tail (tail (init xs))

