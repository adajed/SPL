module SSA (
    toSSA
           ) where

import Control.Monad.Trans.State
import Control.Monad.Identity

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace as Trace

import AbsSPL
import BasicBlock
import CalculateLiveVars ( calculateLiveVars )
import ErrM
import IR

data SSAState = SSAState { vars :: S.Set SVar
                         , lastVar :: M.Map Var Int
                         , varMap :: M.Map Var Var
                         , counter :: Int
                         }

type SSA a = StateT SSAState Err a

initialState :: SSAState
initialState = SSAState { vars = S.empty
                        , lastVar = M.empty
                        , varMap = M.empty
                        , counter = 1
                        }

getNextCounter :: SSA Int
getNextCounter = do
    i <- gets counter
    modify (\s -> s { counter = i + 1 })
    return i

getVarCounter :: Var -> SSA Int
getVarCounter var = do
    m <- gets lastVar
    case m M.!? var of
      Just i -> return i
      Nothing -> fail ("Cannot find var " ++ show var)

incVarCounter :: Var -> SSA ()
incVarCounter var = do
    i <- getNextCounter
    let f = M.insert var i
    modify (\s -> s { lastVar = f (lastVar s) })

addVarIfNew :: Maybe SVar -> SSA ()
addVarIfNew Nothing = return ()
addVarIfNew (Just var) =
    modify (\s -> s { vars = S.insert var (vars s) })

toSSA :: BBGraph -> Err BBGraph
toSSA g = evalStateT (graphToSSA g) initialState

graphToSSA :: BBGraph -> SSA BBGraph
graphToSSA g = do
    let g' = moveArgsToStart g
    let liveVars = M.map head (calculateLiveVars g')
    ids' <- mapM (uncurry basicBlockToSSA) (M.intersectionWith (,) (ids g') liveVars)
    let lastN = M.map snd ids'
    let g'' = g' { ids = M.map fst ids' }
    adjustPhi g'' lastN

moveArgsToStart :: BBGraph -> BBGraph
moveArgsToStart g = g''
    where args = concat (map getArgsBB (M.elems (ids g)))
          f (v@(SVar _ s), n) = IR_Ass v (VarIR (SVar (VarA n) s))
          newargs = map f (zip args [1..])
          getArgsBB (BB _ xs) = concat (map getArgsIR xs)
          getArgsIR ir = case ir of { (IR_Argument x) -> [x] ; _ -> [] }
          isArg ir = case ir of { (IR_Argument _) -> True ; _ -> False }
          removeArgs (BB name xs) = BB name (filter (not . isArg) xs)
          insertArgs (BB name xs) = BB name (newargs ++ xs)
          g' = g { ids = M.map removeArgs (ids g) }
          g'' = g' { ids = M.adjust insertArgs 1 (ids g') }

getVar :: IR -> Maybe SVar
getVar (IR_Ass x _) = Just x
getVar (IR_BinOp _ x _ _) = Just x
getVar (IR_UnOp _ x _) = Just x
getVar (IR_MemRead x _) = Just x
getVar (IR_Call x _ _) = Just x
getVar _ = Nothing

getC :: M.Map Int (M.Map Var Int) -> Int -> Var -> SSA (Int, Var)
getC m i v = do
    x <- liftM (M.! v) $ gets varMap
    unless (M.member i m) (fail "Cannot find i")
    unless (M.member x (m M.! i)) (fail ("Cannot find var " ++ show x))
    let n = (m M.! i) M.! x
    return (i, VarT n)

prevs :: M.Map Int (M.Map Var Int) -> [Int] -> Var -> SSA [(Int, Var)]
prevs m xs v = mapM (\i -> getC m i v) xs

modifyPhi :: M.Map Int (M.Map Var Int) -> [Int] -> IR -> SSA IR
modifyPhi m xs (IR_Phi (SVar v s) _) = do
    ys <- prevs m xs v
    let ys' = map (\(i, v') -> (i, VarIR (SVar v' s))) ys
    return (IR_Phi (SVar v s) ys')
modifyPhi m xs ir = return ir


adjustPhi :: BBGraph -> M.Map Int (M.Map Var Int) -> SSA BBGraph
adjustPhi g lastN = do
    let f (n, (BB name xs)) = do
                              unless (M.member n (prev g)) (fail "Cannot find n")
                              xs' <- mapM (modifyPhi lastN ((prev g) M.! n)) xs
                              return (n, BB name xs')
    ids' <- mapM f (M.assocs (ids g))
    return (g { ids = M.fromList ids' })

basicBlockToSSA :: BasicBlock -> S.Set SVar -> SSA (BasicBlock, M.Map Var Int)
basicBlockToSSA (BB label xs) vars = do
    modify (\s -> s { lastVar = M.empty })
    let phi (SVar var size) = do
            i <- getNextCounter
            modify (\s -> s { lastVar = M.insert var i (lastVar s) })
            modify (\s -> s { varMap = M.insert (VarT i) var (varMap s) })
            return (IR_Phi (SVar (VarT i) size) [])
    xphi <- mapM phi (S.toList vars)
    xs' <- mapM irToSSA xs
    m <- gets lastVar
    return (BB label (xphi ++ xs'), m)


irToSSA :: IR -> SSA IR
irToSSA (IR_Label l) = return (IR_Label l)
irToSSA (IR_Ass x v) = do
    v' <- valueToSSA v
    x' <- varToSSA x
    return (IR_Ass x' v')
irToSSA (IR_BinOp op x v1 v2) = do
    v1' <- valueToSSA v1
    v2' <- valueToSSA v2
    x' <- varToSSA x
    return (IR_BinOp op x' v1' v2')
irToSSA (IR_UnOp op x v) = do
    v' <- valueToSSA v
    x' <- varToSSA x
    return (IR_UnOp op x' v')
irToSSA (IR_MemRead x v) = do
    v' <- valueToSSA v
    x' <- varToSSA x
    return (IR_MemRead x' v')
irToSSA (IR_MemSave v1 v2 size) = do
    v1' <- valueToSSA v1
    v2' <- valueToSSA v2
    return (IR_MemSave v1' v2' size)
irToSSA (IR_Call y f xs) = do
    f' <- valueToSSA f
    xs' <- mapM valueToSSA xs
    y' <- varToSSA y
    return (IR_Call y' f' xs')
irToSSA (IR_VoidCall f xs) = do
    f' <- valueToSSA f
    xs' <- mapM valueToSSA xs
    return (IR_VoidCall f' xs')
irToSSA (IR_Jump l) = return (IR_Jump l)
irToSSA (IR_CondJump v1 op v2 label) = do
    v1' <- valueToSSA v1
    v2' <- valueToSSA v2
    return (IR_CondJump v1' op v2' label)
irToSSA (IR_Return v) = do
    v' <- valueToSSA v
    return (IR_Return v')
irToSSA (IR_VoidReturn) = return IR_VoidReturn
irToSSA (IR_Nop) = return IR_Nop


valueToSSA :: ValIR -> SSA ValIR
valueToSSA v@(VarIR (SVar (VarA _) _)) = return v
valueToSSA (VarIR (SVar var size)) = do
    m <- liftM (M.!? var) $ gets lastVar
    case m of
      Just i -> return (VarIR (SVar (VarT i) size))
      Nothing -> fail $ "Cannot find var: " ++ show var
valueToSSA v = return v

varToSSA :: SVar -> SSA SVar
varToSSA (SVar var size) = do
    incVarCounter var
    i <- getVarCounter var
    modify (\s -> s { varMap = M.insert (VarT i) var (varMap s) })
    return (SVar (VarT i) size)

