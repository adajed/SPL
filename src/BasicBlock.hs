module BasicBlock where

import qualified Data.Map as M

import AbsSPL
import IR
import Token ( VIdent(..) )

data BasicBlock = BB VIdent [IR]
    deriving (Eq)

data BBGraph = G { ids    :: M.Map Int BasicBlock
                 , next   :: M.Map Int [Int]
                 , prev   :: M.Map Int [Int]
                 , layout :: M.Map Int (Maybe Int)
                 , args :: [Int] }
    deriving (Eq)

splitIntoBasicBlocks :: VIdent -> [IR] -> BBGraph
splitIntoBasicBlocks fName ir = buildBBGraph fName (h [] 0 ir)
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
getBasicBlock' (x@(IR_VoidReturn):xs) ys = ((x:ys), xs)
getBasicBlock' (x@(IR_Label _):xs) ys = (ys, x:xs)
getBasicBlock' (ir:xs) ys = getBasicBlock' xs (ir:ys)

buildBBGraph :: VIdent -> [BasicBlock] -> BBGraph
buildBBGraph fName bbs = G { ids = ids', next = next', prev = prev', layout = layout', args = args' }
    where start = VIdent ".__START__"
          bbs' = map (\(BB name xs) -> BB (if name == fName then start else name) xs) bbs
          ids' = M.fromList (zip [1..] bbs')
          n = length bbs'
          layout' = M.mapWithKey (\i _ -> if i == n then Nothing else Just (i+1)) ids'
          ids_rev = M.fromList (zip (map (\(BB name _) -> name) bbs') [1..])
          next' = M.fromList (zip [1..] (map (getNext ids_rev) bbs'))
          prev' = M.fromList (zip [1..] (map (getPrev ids_rev next') bbs'))
          takeArg ir = case ir of { IR_Argument (SVar _ s) -> [s] ; _ -> [] }
          args' = concat (map (\(BB _ xs) -> concat (map takeArg xs)) bbs')

getNext :: M.Map VIdent Int -> BasicBlock -> [Int]
getNext ids (BB name []) =
    let n = ids M.! name
     in if n < (M.size ids) then [n+1] else []
getNext ids (BB name xs) =
    let n = ids M.! name
        next = if n < M.size ids then [n+1] else []
     in case last xs of
          IR_Jump label -> [ids M.! label]
          IR_CondJump _ _ _ label -> (ids M.! label):next
          IR_Return _ -> []
          IR_VoidReturn -> []
          _ -> next

getPrev :: M.Map VIdent Int -> M.Map Int [Int] -> BasicBlock -> [Int]
getPrev ids next (BB name _) =
    let n = ids M.! name
        next' = M.toList next
     in map fst (filter ((elem n) . snd) next')

flattenBBGraph :: BBGraph -> [Int]
flattenBBGraph g = l 1 []
    where l :: Int -> [Int] -> [Int]
          l i acc = case ((layout g) M.! i) of
                      Just j -> l j (i:acc)
                      Nothing -> reverse (i:acc)
