module SSA (
    toSSA,
    removePhi
           ) where

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
toSSA g = runIdentity (evalStateT (graphToSSA g) initialState)

graphToSSA :: BBGraph -> SSA BBGraph
graphToSSA g = do
    let g' = moveArgsToStart g
    setupAllVars g'
    ids' <- mapM basicBlockToSSA (ids g')
    let lastN = Map.map snd ids'
    let g'' = g' { ids = Map.map fst ids' }
    return (adjustPhi g'' lastN)

moveArgsToStart :: BBGraph -> BBGraph
moveArgsToStart g = g''
    where args = concat (Prelude.map getArgsBB (Map.elems (ids g)))
          newargs = Prelude.map (\(x, n) -> IR_Ass x (VArg n)) (Prelude.zip args [1..])
          getArgsBB (BB _ xs) = concat (Prelude.map getArgsIR xs)
          getArgsIR (IR_Argument x) = [x]
          getArgsIR ir = []
          isArg (IR_Argument _) = True
          isArg _ = False
          removeArgs (BB name xs) = BB name (Prelude.filter (not . isArg) xs)
          g' = g { ids = Map.map removeArgs (ids g) }
          insertArgs (BB name xs) = BB name (newargs ++ xs)
          g'' = g' { ids = Map.adjust insertArgs 1 (ids g') }

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
          IR_BinOp _ v _ _ -> h v
          IR_UnOp _ v _    -> h v
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
irToSSA (IR_BinOp op x v1 v2) = do
    v1' <- valueToSSA v1
    v2' <- valueToSSA v2
    x' <- varToSSA x
    return (IR_BinOp op x v1 v2)
irToSSA (IR_UnOp op x v) = do
    v' <- valueToSSA v
    x' <- varToSSA x
    return (IR_UnOp op x' v')
irToSSA (IR_Call y f xs) = do
    y' <- varToSSA y
    f' <- valueToSSA f
    xs' <- mapM valueToSSA xs
    return (IR_Call y' f' xs')
irToSSA (IR_VoidCall f xs) = do
    f' <- valueToSSA f
    xs' <- mapM valueToSSA xs
    return (IR_VoidCall f' xs')
irToSSA (IR_CondJump v1 op v2 label) = do
    v1' <- valueToSSA v1
    v2' <- valueToSSA v2
    return (IR_CondJump v1' op v2' label)
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

removePhi :: BBGraph -> BBGraph
removePhi g = Prelude.foldl f g' phis
    where isPhi (IR_Phi _ _) = True
          isPhi _ = False
          getPhis (BB _ xs) = (Prelude.filter isPhi xs)
          phis = concat (Prelude.map getPhis (Map.elems (ids g)))
          g' = g { ids = Map.map (\(BB name xs) -> BB name (Prelude.filter (not . isPhi) xs)) (ids g) }
          f g (IR_Phi x vs) = insertPhiEquivalence x vs g
          f g _ = g

insertPhiEquivalence :: Var -> [(Int, Value)] -> BBGraph -> BBGraph
insertPhiEquivalence x vs g = Prelude.foldl f g vs
    where f g (i, v) = if v == VVar x then g
                                      else insertAtTheEnd i (IR_Ass x v) g

insertAtTheEnd :: Int -> IR -> BBGraph -> BBGraph
insertAtTheEnd i ir g = g { ids = Map.adjust f i (ids g) }
    where f (BB name []) = BB name [ir]
          f (BB name xs) = case last xs of
                             IR_Jump l -> BB name ((init xs) ++ [ir, IR_Jump l])
                             IR_CondJump v1 op v2 l -> BB name ((init xs) ++ [ir, IR_CondJump v1 op v2 l])
                             _ -> BB name (xs ++ [ir])


