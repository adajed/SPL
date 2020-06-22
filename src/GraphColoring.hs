module GraphColoring (
    colorBBGraph
                     ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import AbsSPL
import BasicBlock
import CalculateLiveVars
import CodeM
import IR
import Token

data Node = NVar SVar | NReg Reg
    deriving (Eq, Ord)

type CollisionGraph = M.Map Node (S.Set Node)

type Coloring = M.Map Node (Maybe Int)

k :: Int
k = length regs

allColors :: [Maybe Int]
allColors = map Just [1..k]

colorBBGraph :: BBGraph -> (BBGraph, M.Map SVar Reg)
colorBBGraph g = colorBBGraph' g S.empty

colorBBGraph' :: BBGraph -> S.Set Node -> (BBGraph, M.Map SVar Reg)
colorBBGraph' g spilledVars =
    let liveVars = calculateLiveVars g
        allVars = getAllVarsBBGraph g
        collisionGraph = calculateCollisionGraph g liveVars
        (collisionGraph', m) = coallesceGraph g collisionGraph
        costs = calculateSpillingCosts g allVars
        colors = color collisionGraph' costs spilledVars
        colors' = recreateColoring colors m
        s = S.fromList (M.keys (M.filter (==Nothing) colors))
      in if isAllColored colors'
            then (g, getColoring colors' (map NVar (S.toList allVars)))
            else colorBBGraph' (spill g colors) (S.union spilledVars s)

getAllVarsBBGraph :: BBGraph -> S.Set SVar
getAllVarsBBGraph g = S.unions (map getAllVarsBB (M.elems (ids g)))
    where getAllVarsBB (BB _ xs) = S.unions (map getAllVarsIR xs)
          getAllVarsIR (IR_Label _) = S.empty
          getAllVarsIR (IR_Ass x v) = getVarsIR [VarIR x, v]
          getAllVarsIR (IR_BinOp _ x v1 v2) = getVarsIR [VarIR x, v1, v2]
          getAllVarsIR (IR_UnOp _ x v) = getVarsIR [VarIR x, v]
          getAllVarsIR (IR_MemRead x v) = getVarsIR [VarIR x, v]
          getAllVarsIR (IR_MemSave v1 v2 _) = getVarsIR [v1, v2]
          getAllVarsIR (IR_Call y f xs) = getVarsIR ((VarIR y):f:xs)
          getAllVarsIR (IR_VoidCall f xs) = getVarsIR (f:xs)
          getAllVarsIR (IR_Return v) = getVarsIR [v]
          getAllVarsIR (IR_VoidReturn) = S.empty
          getAllVarsIR (IR_Jump _) = S.empty
          getAllVarsIR (IR_CondJump v1 _ v2 _) = getVarsIR [v1, v2]
          getAllVarsIR (IR_Nop) = S.empty
          getAllVarsIR (IR_Argument x) = getVarsIR [VarIR x]
          getAllVarsIR (IR_Store x) = getVarsIR [VarIR x]
          getAllVarsIR (IR_Load x) = getVarsIR [VarIR x]

getVarsIR :: [ValIR] -> S.Set SVar
getVarsIR vs = S.unions (map getVar vs)

getVar :: ValIR -> S.Set SVar
getVar (VarIR x) = S.singleton x
getVar _ = S.empty

recreateColoring :: Coloring -> M.Map Node Node -> Coloring
recreateColoring c m = foldl go c (M.keys m)
    where go c' n = M.insert n (findColor c m n) c'

findColor :: Coloring -> M.Map Node Node -> Node -> Maybe Int
findColor c m n = case c M.!? n of
                   Just color -> color
                   Nothing -> findColor c m (m M.! n)

getColoring :: Coloring -> [Node] -> M.Map SVar Reg
getColoring c vs = M.fromList (map f vs)
    where f n@(NVar x) = (x, cols M.! (c M.! n))
          cols = M.fromList (map g (map NReg regs))
          g n@(NReg r) = (c M.! n, r)

calculateCollisionGraph :: BBGraph -> BBLiveVars -> CollisionGraph
calculateCollisionGraph graph liveVars = g''
    where collisions = concat (M.elems liveVars)
          allVars = S.toList (getAllVarsBBGraph graph)
          initMap = M.fromList (zip (map NVar allVars) (repeat S.empty))
          f m c = foldl h m (pairs (map NVar (S.toList c)))
          h m (x1, x2) = ins x1 x2 (ins x2 x1 m)
          ins x y m = M.adjust (S.insert y) x m
          g = foldl f initMap collisions
          g' = foldl argCollisions (insertRegs g) allVars
          g'' = foldl addBBCollision g' (getPairs graph liveVars)

getPairs :: BBGraph -> BBLiveVars -> [(BasicBlock, [S.Set SVar])]
getPairs g m =
    let keys = M.keys (ids g)
        f i = ((ids g) M.! i, m M.! i)
      in map f keys

addBBCollision :: CollisionGraph -> (BasicBlock, [S.Set SVar]) -> CollisionGraph
addBBCollision g (BB _ xs, sets) =
    foldl customCollisions g (zip3 xs (init sets) (tail sets))

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

insertRegs :: CollisionGraph -> CollisionGraph
insertRegs g = foldl f g regs
    where rest r = S.fromList (map NReg (filter (/= r) regs))
          f g' r = M.insert (NReg r) (rest r) g'

argCollisions :: CollisionGraph -> SVar -> CollisionGraph
argCollisions g v@(SVar (VarA n) s) =
    if n > 6 then g
             else let r = argRegs L.!! (n-1)
                      rs = S.fromList (map NReg (filter (/= r) calleeSaveRegs))
                    in M.adjust (S.union rs) (NVar v) g
argCollisions g _ = g

getVars :: [ValIR] -> [SVar]
getVars vs = h vs []
    where h [] acc = acc
          h ((VarIR x):xs) acc = h xs (x:acc)
          h (_:xs) acc = h xs acc

paramCollision :: CollisionGraph -> (ValIR, Int) -> CollisionGraph
paramCollision m (VarIR x, n) = M.adjust (S.union rs) (NVar x) m
    where rn = argRegs L.!! (n - 1)
          rs = S.fromList (map NReg (filter (/= rn) argRegs))
paramCollision m _ = m

customCollisions :: CollisionGraph -> (IR, S.Set SVar, S.Set SVar) -> CollisionGraph
customCollisions m (IR_Call y f xs, set1, set2) =
    let adj m' x = M.adjust (S.insert (NReg ax)) x m'
        set2' = S.delete y set2 -- y can be in ax
        m1 = foldl adj m (map NVar (S.toList set2'))
    in customCollisions m1 (IR_VoidCall f xs, set1, set2)
customCollisions m (IR_VoidCall f xs, set1, set2) =
    let vars1 = map NVar (S.toList (S.intersection set1 set2))
        rs1 = S.fromList (map NReg calleeSaveRegs)
        adj1 m' x = M.adjust (S.union rs1) x m'
        vars2 = map NVar (getVars (f:(drop 6 xs)))
        rs2 = S.fromList (map NReg argRegs)
        adj2 m' x = M.adjust (S.union rs2) x m'
    in foldl paramCollision (foldl adj2 (foldl adj1 m vars1) vars2) (zip xs [1..6])
customCollisions m (IR_BinOp IDiv x v1 v2, set1, set2) =
    let rs = S.fromList [NReg ax, NReg dx]
        adj m' x = M.adjust (S.union rs) x m'
        vars = map NVar ((getVars [v2]) ++ S.toList set2)
     in foldl adj m vars
customCollisions m (IR_BinOp IMod x v1 v2, set1, set2) =
    let rs = S.fromList [NReg ax, NReg dx]
        adj m' x = M.adjust (S.union rs) x m'
        vars = map NVar ((getVars [v2]) ++ S.toList set2)
     in foldl adj m vars
customCollisions m (IR_BinOp ILshift x v1 v2, set1, set2) =
    case getVars [v2] of
      [] -> m
      [y] -> let rs = S.fromList [NReg cx]
                 adj m' x = M.adjust (S.union rs) x m'
                 vars = map NVar (S.toList (S.delete y set2))
               in foldl adj m vars
customCollisions m (IR_BinOp IRshift x v1 v2, set1, set2) =
    case getVars [v2] of
      [] -> m
      [y] -> let rs = S.fromList [NReg cx]
                 adj m' x = M.adjust (S.union rs) x m'
                 vars = map NVar (S.toList (S.delete y set2))
               in foldl adj m vars
customCollisions m _ = m

isAllColored :: M.Map Node (Maybe Int) -> Bool
isAllColored m = M.null (M.filter (== Nothing) m)

color :: CollisionGraph -> M.Map Node Int -> S.Set Node -> Coloring
color g costs spilledVars =
    if not (hasVar g)
        then initColoring
        else let v = case getNode g of
                       Just v' -> v'
                       Nothing -> chooseValueToSpll g costs spilledVars
                 g' = remove v g
                 m  = color g' costs spilledVars
                 c = getColor v g m
              in M.insert v c m

initColoring :: Coloring
initColoring = M.fromList (zip (map NReg regs) allColors)

hasVar :: CollisionGraph -> Bool
hasVar g = (getAllVars g) /= []

getAllVars :: CollisionGraph -> [Node]
getAllVars g = filter isNVar (M.keys g)
    where isNVar (NVar _) = True
          isNVar (NReg _) = False

-- chooses node with highest number of neighbours
chooseValueToSpll :: CollisionGraph -> M.Map Node Int -> S.Set Node -> Node
chooseValueToSpll g costs spilledVars = snd (foldl f (head cs) (tail cs))
    where cs = map (\n -> (S.size (g M.! n), n)) (filter (`S.notMember` spilledVars) (getAllVars g))
          f (ix, x) (iy, y) = if iy > ix then (iy, y) else (ix, x)

getNode :: CollisionGraph -> Maybe Node
getNode g = maybe_head ys
    where xs = M.keys (M.filter ((<k) . S.size) g)
          ys = filter f xs
          f (NVar _) = True
          f (NReg _) = False

remove :: Node -> CollisionGraph -> CollisionGraph
remove v g = M.delete v (M.map (S.delete v) g)

getColor :: Node -> CollisionGraph -> Coloring -> Maybe Int
getColor v g m = let vs = g M.! v
                     cs = map (m M.!) (S.toList vs)
                     allowedCs = filter (`notElem` cs) allColors
                  in case maybe_head allowedCs of
                       Just c -> c
                       Nothing -> Nothing

maybe_head :: [a] -> Maybe a
maybe_head [] = Nothing
maybe_head (x:xs) = Just x

spill :: BBGraph -> Coloring -> BBGraph
spill g cols = foldl spillVar g varsToSpill
    where varsToSpill = M.keys (M.filter (== Nothing) cols)

spillVar :: BBGraph -> Node -> BBGraph
spillVar g (NVar x@(SVar (VarA n) _)) = g { ids = ids' }
    where ids' = M.map f (ids g)
          f (BB (VIdent ".__START__") xs) = BB (VIdent ".__START__") c
            where c = if n < 7
                         then [IR_Store x] ++ (concat (map (spillIR x) xs))
                         else concat (map (spillIR x) xs)
          f (BB name xs) = BB name (concat (map (spillIR x) xs))
spillVar g (NVar x) = g { ids = ids' }
    where ids' = M.map f (ids g)
          f (BB name xs) = BB name (concat (map (spillIR x) xs))
spillVar g _ = g

spillIR :: SVar -> IR -> [IR]
spillIR x ir@(IR_Store _) = [ir]
spillIR x ir@(IR_Load _) = [ir]
spillIR x ir =
    if S.member x (uses ir)
       then if S.member x (kill ir)
               then [IR_Load x, ir, IR_Store x]
               else [IR_Load x, ir]
       else if S.member x (kill ir)
               then [ir, IR_Store x]
               else [ir]

calculateSpillingCosts :: BBGraph -> S.Set SVar -> M.Map Node Int
calculateSpillingCosts g vars = M.fromList (map f (map NVar (S.toList vars)))
    where f n@(NVar x) = (n, calculateSpillCostForVar g x)

calculateSpillCostForVar :: BBGraph -> SVar -> Int
calculateSpillCostForVar g x = sum (map costBB (M.elems (ids g)))
    where cost ir = let n1 = if S.member x (uses ir) then 1 else 0
                        n2 = if S.member x (kill ir) then 1 else 0
                     in n1 + n2
          costBB (BB _ xs) = sum (map cost xs)

coallesceGraph :: BBGraph -> CollisionGraph -> (CollisionGraph, M.Map Node Node)
coallesceGraph g c = foldl go (c, M.empty) xs
    where xs = getIRAss g
          go (coll, m) (x, y) =
              if (M.notMember x coll) || (M.notMember y coll)
                 then (coll, m)
                 else if S.notMember x (coll M.! y)
                         then let sc = (S.union (coll M.! x) (coll M.! y)) S.\\ (S.fromList [x, y])
                                  n = S.filter ((>k) . S.size . (coll M.!)) sc
                               in if S.size n < k
                                     then let coll1 = M.delete y coll
                                              coll2 = M.insert x sc coll1
                                              coll3 = replace x y coll2
                                              m1 = M.insert y x m
                                           in (coll3, m1)
                                     else (coll, m)
                         else (coll, m)

replace :: Node -> Node -> CollisionGraph -> CollisionGraph
replace x y coll = M.map go coll
    where go set = if S.member y set
                      then S.insert x (S.delete y set)
                      else set

getIRAss :: BBGraph -> [(Node, Node)]
getIRAss g = concat (map getIRAssBB (M.elems (ids g)))
    where getIRAssBB (BB _ xs) = concat (map go xs)
          go (IR_Ass x (VarIR y)) = [(NVar x, NVar y)]
          go _ = []
