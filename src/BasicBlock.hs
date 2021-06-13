module BasicBlock where

import qualified Data.Map as M
import qualified Data.List as L

import AbsSPL
import IR
import Token ( VIdent(..) )

data BasicBlock = BB { bbLabel :: VIdent
                     , bbCode  :: [IR]
                     }
    deriving (Eq)
instance Show BasicBlock where
    show bb = show (bbLabel bb) ++ "\n" ++ (L.intercalate "\n" (map show (bbCode bb)))

data BBGraph = G { ids    :: M.Map Int BasicBlock
                 , next   :: M.Map Int [Int]
                 , prev   :: M.Map Int [Int]
                 , layout :: [Int]
                 , args :: [Int]
                 , start :: Int
                 , end :: [Int]
                 }
    deriving (Eq)

instance Show BBGraph where
    show graph = L.intercalate "\n\n\n" (map f (layout graph))
        where f i = show i ++ "\n" ++ show ((ids graph) M.! i) ++ "\nnext: " ++ show ((next graph) M.! i) ++ "\nprev: " ++ show ((prev graph) M.! i)

splitIntoBasicBlocks :: VIdent -> [IR] -> BBGraph
splitIntoBasicBlocks fName ir = buildBBGraph fName (h [] (getMaxLabel ir) ir)
    where h bbs _ [] = reverse bbs
          h bbs n xs = h ((BB name bb):bbs) n' xs'
            where (name, bb, xs', n') = getBasicBlock xs n
          getMaxLabel :: [IR] -> Int
          getMaxLabel xs = maximum (0:(map f xs))
              where f (IR_Label (VIdent name)) = if L.isPrefixOf ".L" name then (read (drop 2 name)) + 1 else 0
                    f _ = 0

getBasicBlock :: [IR] -> Int -> (VIdent, [IR], [IR], Int)
getBasicBlock ((IR_Label name):xs) n = (name, reverse bb, xs', n)
    where (bb, xs') = getBasicBlock' xs []
getBasicBlock xs n = (VIdent (".L" ++ show n), reverse bb, xs', n + 1)
    where (bb, xs') = getBasicBlock' xs []

getBasicBlock' :: [IR] -> [IR] -> ([IR], [IR])
getBasicBlock' [] ys = (ys, [])
getBasicBlock' (x@(IR_Jump _):xs) ys = ((x:ys), xs)
getBasicBlock' (x@(IR_CondJump _ _ _ _ _):xs) ys = ((x:ys), xs)
getBasicBlock' (x@(IR_Return _):xs) ys = ((x:ys), xs)
getBasicBlock' (x@(IR_VoidReturn):xs) ys = ((x:ys), xs)
getBasicBlock' (x@(IR_Label _):xs) ys = (ys, x:xs)
getBasicBlock' (ir:xs) ys = getBasicBlock' xs (ir:ys)

isReturn :: [IR] -> Bool
isReturn [] = False
isReturn xs = case last xs of
                IR_Return _ -> True
                IR_VoidReturn -> True
                _ -> False


isJump :: [IR] -> Bool
isJump [] = False
isJump xs = case last xs of
              IR_Return _ -> True
              IR_VoidReturn -> True
              IR_Jump _ -> True
              _ -> False


getMissingJump_ :: BBGraph -> Int -> [Int]
getMissingJump_ bbgraph i =
    let code = bbCode ((ids bbgraph) M.! i)
     in case code of
          [] -> (next bbgraph) M.! i
          _ -> case last code of
                 IR_Return _ -> []
                 IR_VoidReturn -> []
                 IR_Jump _ -> []
                 IR_CondJump _ _ _ _ _ -> []
                 _ -> (next bbgraph) M.! i

fixJumps :: BBGraph -> BBGraph
fixJumps bbgraph = bbgraph { ids = ids' }
    where xs = M.fromList (map (\i -> (i, getMissingJump_ bbgraph i)) (M.keys (ids bbgraph)))
          f i bb = bb { bbCode = (bbCode bb) ++ (map (\j -> IR_Jump (bbLabel ((ids bbgraph) M.! j))) (xs M.! i)) }
          ids' = M.mapWithKey f (ids bbgraph)


buildBBGraph :: VIdent -> [BasicBlock] -> BBGraph
buildBBGraph fName bbs = fixJumps (buildBBGraph_ fName bbs)


buildBBGraph_ :: VIdent -> [BasicBlock] -> BBGraph
buildBBGraph_ fName bbs = G { ids = ids', next = next', prev = prev', layout = layout', args = args', start = start', end = end' }
    where startIdent = VIdent ".__START__"
          bbs' = map (\(BB name xs) -> BB (if name == fName then startIdent else name) xs) bbs
          ids' = M.fromList (zip [1..] bbs')
          start' = fst $ head $ filter ((startIdent==) . bbLabel . snd) (M.assocs ids')
          end' = map fst $ filter (isReturn . bbCode . snd) (M.assocs ids')
          n = length bbs'
          layout' = map fst (zip [1..] bbs')
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
          IR_CondJump _ _ _ label1 label2 -> [ids M.! label1, ids M.! label2]
          IR_Return _ -> []
          IR_VoidReturn -> []
          _ -> next

getPrev :: M.Map VIdent Int -> M.Map Int [Int] -> BasicBlock -> [Int]
getPrev ids next (BB name _) =
    let n = ids M.! name
        next' = M.toList next
     in map fst (filter ((elem n) . snd) next')
