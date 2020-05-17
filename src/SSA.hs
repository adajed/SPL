module SSA where

import Control.Monad.Trans.State
import Control.Monad.Identity

import Data.Map as Map

import Debug.Trace as Trace

import AbsSPL
import BasicBlock
import IR

data SSAState = SSAState { varCounter :: Map Ident Int
                         , lastCounter :: Map Ident Int
                         }

type SSA a = StateT SSAState Identity a

initialState :: SSAState
initialState = SSAState { varCounter = Map.empty
                        , lastCounter = Map.empty}

vars :: SSA [Ident]
vars = liftM Map.keys (gets varCounter)

incrementNextCnt :: Ident -> SSA ()
incrementNextCnt x =
    modify (\s -> s { varCounter = Map.adjust (+1) x (varCounter s) })

getNextVarCnt :: Ident -> SSA Int
getNextVarCnt x = do
    n <- liftM (!x) $ gets varCounter
    incrementNextCnt x
    return n

getCurrentCnt :: Ident -> SSA Int
getCurrentCnt x = liftM (!x) $ gets lastCounter

addVarIfNew :: Maybe Ident -> SSA ()
addVarIfNew Nothing = return ()
addVarIfNew (Just name) = do
    map <- gets varCounter
    unless (Map.member name map) (
        modify (\s -> s { varCounter = Map.insert name 0 map }))

toSSA :: BBGraph -> BBGraph
toSSA g = Trace.traceShow (Map.keys (varCounter s)) g'
    where (g', s) = runIdentity (runStateT (graphToSSA g) initialState)

graphToSSA :: BBGraph -> SSA BBGraph
graphToSSA g = do
    setupAllVars g
    ids' <- mapM basicBlockToSSA (ids g)
    let lastN = Map.map snd ids'
    let g' = g { ids = Map.map fst ids' }
    return (adjustPhi g' lastN)

setupAllVars :: BBGraph -> SSA ()
setupAllVars g = mapM_ setupVars (Map.elems (ids g))
    where setupVars (BB _ xs) = mapM_ addVarIfNew (Prelude.map getVar xs)

getVar :: IR -> Maybe Ident
getVar ir =
    let h :: Var -> Maybe Ident
        h (VarN name) = Just name
        h (VarT _) = Nothing
     in case ir of
          IR_Ass v _        -> h v
          IR_IBinOp _ v _ _ -> h v
          IR_IUnOp _ v _    -> h v
          IR_BBinOp _ v _ _ -> h v
          IR_BUnOp _ v _    -> h v
          IR_IRelOp _ v _ _ -> h v
          IR_Call v _ _     -> h v
          _ -> Nothing

adjustPhi :: BBGraph -> Map Int (Map Ident Int) -> BBGraph
adjustPhi g lastN = g { ids = ids' }
    where ids' = Map.mapWithKey f (ids g)
          f n (BB name xs) = BB name (Prelude.map (p n) xs)
          p n (IR_Phi (VarC x t) _) = IR_Phi (VarC x t) prevs
              where prevs = Prelude.map h ((prev g) ! n)
                    h i = (i, VVar (VarC x ((lastN ! i) ! x)))
          p n ir = ir

insertPhi :: BBGraph -> SSA BBGraph
insertPhi g = do
    v <- vars
    let h x = do
            n <- getNextVarCnt x
            return (IR_Phi (VarC x n) [])
    let m (BB name xs) = do
            xs' <- mapM h v
            return (BB name (xs' ++ xs))
    ids' <- mapM m (ids g)
    return (g { ids = ids'})

basicBlockToSSA :: BasicBlock -> SSA (BasicBlock, Map Ident Int)
basicBlockToSSA (BB label xs) = do
    v <- vars
    modify (\s -> s { lastCounter = Map.empty })
    let phi x = do
            n <- getNextVarCnt x
            modify (\s -> s { lastCounter = Map.insert x n (lastCounter s) })
            return (IR_Phi (VarC x n) [])
    xphi <- mapM phi v
    xs' <- mapM irToSSA xs
    m <- gets lastCounter
    return (BB label (xphi ++ xs'), m)


irToSSA :: IR -> SSA IR
irToSSA (IR_Ass x v) = do
    v' <- valueToSSA v
    x' <- varToSSA x
    return (IR_Ass x' v')
irToSSA (IR_IBinOp op x v1 v2) = do
    v1' <- valueToSSA v1
    v2' <- valueToSSA v2
    x' <- varToSSA x
    return (IR_IBinOp op x v1 v2)
irToSSA (IR_IUnOp op x v) = do
    v' <- valueToSSA v
    x' <- varToSSA x
    return (IR_IUnOp op x' v')
irToSSA (IR_BBinOp op x v1 v2) = do
    v1' <- valueToSSA v1
    v2' <- valueToSSA v2
    x' <- varToSSA x
    return (IR_BBinOp op x v1 v2)
irToSSA (IR_BUnOp op x v) = do
    v' <- valueToSSA v
    x' <- varToSSA x
    return (IR_BUnOp op x' v')
irToSSA (IR_IRelOp op x v1 v2) = do
    v1' <- valueToSSA v1
    v2' <- valueToSSA v2
    x' <- varToSSA x
    return (IR_IRelOp op x v1 v2)
irToSSA (IR_Param v) = do
    v' <- valueToSSA v
    return (IR_Param v')
irToSSA (IR_CondJump v label) = do
    v' <- valueToSSA v
    return (IR_CondJump v' label)
irToSSA (IR_Return v) = do
    v' <- valueToSSA v
    return (IR_Return v')
irToSSA ir = return ir

valueToSSA :: Value -> SSA Value
valueToSSA (VVar (VarN name)) = do
    n <- liftM (!name) $ gets lastCounter
    return (VVar (VarC name n))
valueToSSA v = return v

varToSSA :: Var -> SSA Var
varToSSA (VarN name) = do
    m <- gets lastCounter
    let n = m ! name
    modify (\s -> s { lastCounter = Map.insert name (n+1) m })
    incrementNextCnt name
    return (VarC name (n+1))
varToSSA v = return v

