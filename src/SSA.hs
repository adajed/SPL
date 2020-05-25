module SSA (
    toSSA,
    removePhi
           ) where

import Control.Monad.Trans.State
import Control.Monad.Identity

import Data.Map as Map
import Data.Set as Set

import Debug.Trace as Trace

import AbsSPL
import BasicBlock
import IR

data SSAState = SSAState { vars :: Set SVar
                         , lastVar :: Map Var Int
                         , counter :: Int
                         }

type SSA a = StateT SSAState Identity a

initialState :: SSAState
initialState = SSAState { vars = Set.empty
                        , lastVar = Map.empty
                        , counter = 1
                        }

getNextCounter :: SSA Int
getNextCounter = do
    i <- gets counter
    modify (\s -> s { counter = i + 1 })
    return i

getVarCounter :: Var -> SSA Int
getVarCounter var = liftM (!var) $ gets lastVar

incVarCounter :: Var -> SSA ()
incVarCounter var = do
    i <- getNextCounter
    let f = Map.insert var i
    modify (\s -> s { lastVar = f (lastVar s) })

addVarIfNew :: Maybe SVar -> SSA ()
addVarIfNew Nothing = return ()
addVarIfNew (Just var) =
    modify (\s -> s { vars = Set.insert var (vars s) })

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
          f (v@(SVar _ s), n) = IR_Ass v (VarIR (SVar (VarA n) s))
          newargs = Prelude.map f (Prelude.zip args [1..])
          getArgsBB (BB _ xs) = concat (Prelude.map getArgsIR xs)
          getArgsIR ir = case ir of { (IR_Argument x) -> [x] ; _ -> [] }
          isArg ir = case ir of { (IR_Argument x) -> True ; _ -> False }
          removeArgs (BB name xs) = BB name (Prelude.filter (not . isArg) xs)
          g' = g { ids = Map.map removeArgs (ids g) }
          insertArgs (BB name xs) = BB name (newargs ++ xs)
          g'' = g' { ids = Map.adjust insertArgs 1 (ids g') }

setupAllVars :: BBGraph -> SSA ()
setupAllVars g = mapM_ setupVars (Map.elems (ids g))
    where setupVars (BB _ xs) = mapM_ (addVarIfNew . getVar) xs

getVar :: IR -> Maybe SVar
getVar (IR_Ass x _) = Just x
getVar (IR_BinOp _ x _ _) = Just x
getVar (IR_UnOp _ x _) = Just x
getVar (IR_MemRead x _) = Just x
getVar (IR_Call x _ _) = Just x
getVar _ = Nothing

adjustPhi :: BBGraph -> Map Int (Map Var Int) -> BBGraph
adjustPhi g lastN = g { ids = ids' }
    where ids' = Map.mapWithKey f (ids g)
          f n (BB name xs) = BB name (Prelude.map (p n) xs)
          p n (IR_Phi v@(SVar var size) _) = IR_Phi v prevs
              where prevs = Prelude.map h ((prev g) ! n)
                    h i = (i, VarIR (SVar (VarT ((lastN ! i) ! var)) size))
          p n ir = ir

basicBlockToSSA :: BasicBlock -> SSA (BasicBlock, Map Var Int)
basicBlockToSSA (BB label xs) = do
    vs <- liftM Set.toList $ gets vars
    modify (\s -> s { lastVar = Map.empty })
    let phi (SVar var size) = do
            i <- getNextCounter
            modify (\s -> s { lastVar = Map.insert var i (lastVar s) })
            return (IR_Phi (SVar (VarT i) size) [])
    xphi <- mapM phi vs
    xs' <- mapM irToSSA xs
    m <- gets lastVar
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

valueToSSA :: ValIR -> SSA ValIR
valueToSSA (VarIR (SVar var size)) = do
    i <- liftM (!var) $ gets lastVar
    return (VarIR (SVar (VarT i) size))
valueToSSA v = return v

varToSSA :: SVar -> SSA SVar
varToSSA (SVar var size) = do
    incVarCounter var
    i <- getVarCounter var
    return (SVar (VarT i) size)

removePhi :: BBGraph -> BBGraph
removePhi g = Prelude.foldl f g' phis
    where isPhi (IR_Phi _ _) = True
          isPhi _ = False
          getPhis (BB _ xs) = (Prelude.filter isPhi xs)
          phis = concat (Prelude.map getPhis (Map.elems (ids g)))
          g' = g { ids = Map.map (\(BB name xs) -> BB name (Prelude.filter (not . isPhi) xs)) (ids g) }
          f g (IR_Phi x vs) = insertPhiEquivalence x vs g
          f g _ = g

insertPhiEquivalence :: SVar -> [(Int, ValIR)] -> BBGraph -> BBGraph
insertPhiEquivalence x vs g = Prelude.foldl f g vs
    where f g (i, v) = if v == VarIR x then g
                                      else insertAtTheEnd i (IR_Ass x v) g

insertAtTheEnd :: Int -> IR -> BBGraph -> BBGraph
insertAtTheEnd i ir g = g { ids = Map.adjust f i (ids g) }
    where f (BB name []) = BB name [ir]
          f (BB name xs) = case last xs of
                             IR_Jump l -> BB name ((init xs) ++ [ir, IR_Jump l])
                             IR_CondJump v1 op v2 l -> BB name ((init xs) ++ [ir, IR_CondJump v1 op v2 l])
                             _ -> BB name (xs ++ [ir])


