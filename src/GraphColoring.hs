module GraphColoring (
    colorBBGraph
                     ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import BasicBlock
import CalculateLiveVars
import CodeM
import IR

data Node = NVar SVar | NReg Reg
    deriving (Eq, Ord)

type CollisionGraph = M.Map Node (S.Set Node)

type Coloring = M.Map Node (Maybe Int)

regs :: [Reg]
regs = [ax, cx, dx, di, si, r8, r9, r10, r11, bx, r12, r13, r14, r15]

calleeSaveRegs :: [Reg]
calleeSaveRegs = [ax, cx, dx, di, si, r8, r9, r10, r11]

callerSaveRegs :: [Reg]
callerSaveRegs = [bx, r12, r13, r14, r15]

argRegs :: [Reg]
argRegs = [di, si, dx, cx, r8, r9]

k :: Int
k = length regs

allColors :: [Maybe Int]
allColors = map Just [1..k]

colorBBGraph :: BBGraph -> (BBGraph, M.Map SVar Reg)
colorBBGraph g =
    let liveVars = calculateLiveVars g
        allVars = S.unions (M.elems (M.map S.unions liveVars))
        collisionGraph = calculateCollisionGraph g liveVars
        costs = calculateSpillingCosts g allVars
        colors = color collisionGraph costs
      in if isAllColored colors
            then (g, getColoring colors (map NVar (S.toList allVars)))
            else let g' = spill g colors
                  in colorBBGraph g'

getColoring :: Coloring -> [Node] -> M.Map SVar Reg
getColoring c vs = M.fromList (map f vs)
    where f n@(NVar x) = (x, cols M.! (c M.! n))
          cols = M.fromList (map g (map NReg regs))
          g n@(NReg r) = (c M.! n, r)

calculateCollisionGraph :: BBGraph -> BBLiveVars -> CollisionGraph
calculateCollisionGraph graph liveVars = g''
    where collisions = concat (M.elems liveVars)
          allVars = S.toList (S.unions collisions)
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
                      rs = S.fromList (map NReg (filter (/= r) argRegs))
                    in M.adjust (S.union rs) (NVar v) g
argCollisions g _ = g

getVars :: [ValIR] -> [SVar]
getVars vs = h vs []
    where h [] acc = acc
          h ((VarIR x):xs) acc = h xs (x:acc)
          h (_:xs) acc = h xs acc

customCollisions :: CollisionGraph -> (IR, S.Set SVar, S.Set SVar) -> CollisionGraph
customCollisions m (IR_Call y f xs, set1, set2) =
    let adj m' x = M.adjust (S.insert (NReg ax)) x m'
        set2' = S.delete y set2 -- y can be in ax
        m1 = foldl adj m (map NVar (S.toList set2'))
    in customCollisions m1 (IR_VoidCall f xs, set1, set2)
customCollisions m (IR_VoidCall _ _, set1, set2) =
    let ys = S.toList (S.intersection set1 set2)
        vars = map NVar ys
        rs' = S.fromList (map NReg calleeSaveRegs)
        adj m' x = M.adjust (S.union rs') x m'
    in foldl adj m vars
customCollisions m (IR_BinOp (BOpInt IDiv) x v1 v2, set1, set2) =
    let rs = S.fromList [NReg ax, NReg dx]
        adj m' x = M.adjust (S.union rs) x m'
        vars = map NVar ((getVars [v2]) ++ S.toList set2)
     in foldl adj m vars
customCollisions m (IR_BinOp (BOpInt IMod) x v1 v2, set1, set2) =
    let rs = S.fromList [NReg ax, NReg dx]
        adj m' x = M.adjust (S.union rs) x m'
        vars = map NVar ((getVars [v2]) ++ S.toList set2)
     in foldl adj m vars
customCollisions m (IR_BinOp (BOpInt ILshift) x v1 v2, set1, set2) =
    case getVars [v2] of
      [] -> m
      [y] -> let rs = S.fromList [NReg cx]
                 adj m' x = M.adjust (S.union rs) x m'
                 vars = map NVar (S.toList (S.delete y set2))
               in foldl adj m vars
customCollisions m (IR_BinOp (BOpInt IRshift) x v1 v2, set1, set2) =
    case getVars [v2] of
      [] -> m
      [y] -> let rs = S.fromList [NReg cx]
                 adj m' x = M.adjust (S.union rs) x m'
                 vars = map NVar (S.toList (S.delete y set2))
               in foldl adj m vars
customCollisions m _ = m

isAllColored :: M.Map Node (Maybe Int) -> Bool
isAllColored m = M.null (M.filter (== Nothing) m)

color :: CollisionGraph -> M.Map Node Int -> Coloring
color g costs =
    if not (hasVar g)
        then initColoring
        else let v = case getNode g of
                       Just v' -> v'
                       Nothing -> takeMinCost g costs
                 g' = remove v g
                 m  = color g' costs
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

takeMinCost :: CollisionGraph -> M.Map Node Int -> Node
takeMinCost g costs = snd (foldl f (head cs) (tail cs))
    where cs = map (\n -> (costs M.! n, n)) (getAllVars g)
          f (ix, x) (iy, y) = if iy < ix then (iy, y) else (ix, x)

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

