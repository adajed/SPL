module InlineFunctions (
    inlineFunctions
                       ) where


import BasicBlock
import BuiltInFunctions ( builtInFunctions )
import IR
import OptimizationUtils
import Token ( VIdent(..) )
import ErrM

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Bool (bool)

import Control.Monad
import Control.Monad.Trans.State

import Debug.Trace (trace, traceShowId)

data StateM = StateM { canInline :: M.Map VIdent Bool
                     , bbgraph   :: M.Map VIdent BBGraph
                     , labelCounter :: Int
                     , labelMapping :: M.Map VIdent (M.Map VIdent VIdent)
                     , arguments :: M.Map Int ValIR
                     , tempOffset :: Int
                     }
type InlineM a = StateT StateM Err a


mapWithKeyM :: Ord k => (k -> a -> InlineM b) -> M.Map k a -> InlineM (M.Map k b)
mapWithKeyM f v = liftM M.fromList (mapM f' (M.assocs v))
    where f' (x, y) = f x y >>= \y' -> return (x, y')


getNextLabel :: InlineM VIdent
getNextLabel = do
    l <- gets labelCounter
    modify (\s -> s { labelCounter = (labelCounter s) + 1 })
    return (VIdent (".L" ++ show l))


checkCanInline :: VIdent -> InlineM Bool
checkCanInline name = do
    m <- gets canInline
    return (case m M.!? name of { Just b -> b;  Nothing -> False })


max_ :: [Int] -> Int
max_ [] = 0
max_ xs = maximum xs


getMaxTempVariable :: VIdent -> InlineM Int
getMaxTempVariable name = do
    g <- gets ((M.!? name) . bbgraph)
    case g of
      Nothing -> return 0
      Just graph -> let getMaxTempVariable' (BB _ xs) = max_ (map getTempId (concat (map getAllVars xs)))
                        getTempId (SVar (VarT n) _) = n
                        getTempId _ = 0
                     in return (max_ (M.elems (M.map getMaxTempVariable' (ids graph))))

initialStateM program = StateM { canInline = M.map canInlineFunction program
                               , bbgraph = program
                               , labelCounter = maximum (0:(M.elems (M.map getMaxLabel program)))
                               , labelMapping = M.map (const M.empty) program
                               , arguments = M.empty
                               , tempOffset = 0
                               }


inlineFunctions :: M.Map VIdent BBGraph -> M.Map VIdent BBGraph
inlineFunctions program =
    case evalStateT (mapWithKeyM inlineBBGraph program) (initialStateM program) of
      Bad errorMsg -> trace ("kupa: " ++ errorMsg) program
      Ok newProgram -> newProgram



getMaxLabel :: BBGraph -> Int
getMaxLabel g = maximum (0:(M.elems (M.map getMaxLabel' (ids g))))
    where getMaxLabel' :: BasicBlock -> Int
          getMaxLabel' (BB (VIdent name) _) = if L.isPrefixOf ".L" name then (read (drop 2 name)) + 1 else 0

liftBBf :: ([IR] -> a) -> BasicBlock -> a
liftBBf f (BB name xs) = (f xs)

canInlineFunction :: BBGraph -> Bool
canInlineFunction g = not (any (liftBBf (any isCall)) (M.elems (ids g)))
    where isCall :: IR -> Bool
          isCall (IR_Call _ (LabelIR name) _) = not (elem name builtInFunctions)
          isCall (IR_Call _ _ _) = True
          isCall (IR_VoidCall (LabelIR name) _) = not (elem name builtInFunctions)
          isCall (IR_VoidCall _ _) = True
          isCall _ = False


maybeM :: InlineM a -> (b -> InlineM a) -> Maybe b -> InlineM a
maybeM m f Nothing = m
maybeM m f (Just x) = f x


inlineBBGraph :: VIdent -> BBGraph -> InlineM BBGraph
inlineBBGraph name graph = do
    m <- getInlinableFunction graph
    case m of
      Nothing -> return graph
      Just p -> inlineFunction_ name graph p -- >>= inlineBBGraph name

takeInfo :: IR -> (Maybe SVar, VIdent, [ValIR])
takeInfo (IR_Call y (LabelIR name) xs) = (Just y, name, xs)
takeInfo (IR_VoidCall (LabelIR name) xs) = (Nothing, name, xs)


inlineFunction_ :: VIdent -> BBGraph -> (Int, Int) -> InlineM BBGraph
inlineFunction_ nameDest graph (bbIdx, lineIdx) = do
    newLabel <- getNextLabel
    let bb = (ids graph) M.! bbIdx
    let code = bbCode bb
    let start_code = take lineIdx code
    let bb_end = BB { bbLabel = newLabel, bbCode = drop (lineIdx + 1) code }
    let (y, nameSrc, xs) = takeInfo ((bbCode bb) !! lineIdx)
    modify (\s -> s { arguments = M.fromList (zip [1..] xs) })
    let prevNodes = (prev graph) M.! bbIdx
    let nextNodes = (next graph) M.! bbIdx
    g <- gets ((M.! nameSrc) . bbgraph)
    g' <- prepareBBGraphToInserting nameDest nameSrc g
    let g'' = g' { ids = M.adjust (\b -> b { bbCode = start_code ++ (bbCode b) }) (start g') (ids g') }
    let g''' = insertNewEnd g'' bb_end y
    when (length (end g''') > 1) (fail "length (end g''') > 1")
    let newGraph = mergeGraphs graph g''' bbIdx
    return newGraph



getInlinableFunction :: BBGraph -> InlineM (Maybe (Int, Int))
getInlinableFunction graph = help (M.assocs (ids graph))
    where help :: [(Int, BasicBlock)] -> InlineM (Maybe (Int, Int))
          help [] = return Nothing
          help ((i, bb):bbs) = do
              m <- help2 0 (bbCode bb)
              case m of
                Nothing -> help bbs
                Just j -> return (Just (i, j))
          help2 :: Int -> [IR] -> InlineM (Maybe Int)
          help2 j [] = return Nothing
          help2 j (x:xs) =
              case x of
                IR_Call _ (LabelIR name) _ -> do
                    b <- checkCanInline name
                    if b then return (Just j) else help2 (j+1) xs
                IR_VoidCall (LabelIR name) _ -> do
                    b <- checkCanInline name
                    if b then return (Just j) else help2 (j+1) xs
                _ -> help2 (j+1) xs



renameVariables :: BasicBlock -> InlineM BasicBlock
renameVariables bb = do
    offset <- gets tempOffset
    argMapping <- gets arguments
    let offsetTempVar (SVar (VarT n) size) = SVar (VarT (n + offset)) size
        offsetTempVar svar = svar
    let g value = case value of
                    VarIR (SVar (VarA n) size) -> argMapping M.! n
                    VarIR svar                 -> VarIR (offsetTempVar svar)
                    _                          -> value
    let f = (modifyVar offsetTempVar) . (modifyValue g)
    return (bb { bbCode = map f (bbCode bb) })



prepareBBGraphToInserting :: VIdent -> VIdent -> BBGraph -> InlineM BBGraph
prepareBBGraphToInserting nameDest nameSrc g = do
    n <- getMaxTempVariable nameDest
    modify (\s -> s { tempOffset = n })
    modify (\s -> s { labelMapping = M.insert nameSrc M.empty (labelMapping s) })
    g' <- renameLabels nameSrc g
    ids' <- mapM renameVariables (ids g')
    return (g' { ids = ids' })


renameLabels :: VIdent -> BBGraph -> InlineM BBGraph
renameLabels name bbgraph = do
    let computeNewName fname lname@(VIdent label) =
            do newName <- getNextLabel
               let updateFun = M.insert lname newName
               modify (\s -> s { labelMapping = M.adjust updateFun fname (labelMapping s) } )
    mapM_ (computeNewName name) (M.map bbLabel (ids bbgraph))
    ids' <- mapM (updateLabels name) (ids bbgraph)
    return (bbgraph { ids = ids' })


updateLabels :: VIdent -> BasicBlock -> InlineM BasicBlock
updateLabels fname bb = do
    mapping <- gets ((M.! fname) . labelMapping)
    let saveGet m x = case m M.!? x of { Just y -> y; Nothing -> x }
    let bblabel' = saveGet mapping (bbLabel bb)
    let bbcode' = map (modifyLabel (saveGet mapping)) (bbCode bb)
    return (BB { bbLabel = bblabel', bbCode = bbcode' })



insertNewBB :: BBGraph -> BasicBlock -> (BBGraph, Int)
insertNewBB bbgraph bb = (bbgraph { ids = ids', prev = prev', next = next', layout = layout' }, newId)
    where newId = maximum (M.keys (ids bbgraph)) + 1
          ids' = M.insert newId bb (ids bbgraph)
          prev' = M.insert newId [] (prev bbgraph)
          next' = M.insert newId [] (next bbgraph)
          layout' = (layout bbgraph) ++ [newId]


insertNewEnd :: BBGraph -> BasicBlock -> Maybe SVar -> BBGraph
insertNewEnd g bb x = g' { ids = ids'', prev = prev', next = next', end = [newId] }
    where (g', newId) = insertNewBB g bb
          prev' = M.insert newId (end g') (prev g')
          next' = M.mapWithKey (\k v -> if elem k (end g') then [newId] else v) (next g')
          getRetValue (IR_Return v) = v
          getRetValue (IR_VoidReturn) = IntIR 0 4
          getPhiValues i = (i, getRetValue (last (bbCode ((ids g) M.! i))))
          returnValues = map getPhiValues (end g)
          ids' = foldl (\m i -> M.adjust (\b -> b { bbCode = init (bbCode b) ++ [IR_Jump (bbLabel bb)] }) i m) (ids g') (end g)
          ids'' = case x of
                    Nothing -> ids'
                    Just svar -> M.adjust (\b -> b { bbCode = (IR_Phi svar returnValues):(bbCode b)}) newId ids'


renameIds :: (Int -> Int) -> BBGraph -> BBGraph
renameIds f bbgraph = G { ids = ids'', prev = prev', next = next', layout = layout', args = args', start = start', end = end' }
    where ids' = M.mapKeys f (ids bbgraph)
          f' (IR_Phi x vs) = IR_Phi x (map (\(i, v) -> (f i, v)) vs)
          f' ir = ir
          ids'' = M.map (\bb -> bb { bbCode = map f' (bbCode bb) }) ids'
          prev' = M.map (map f) (M.mapKeys f (prev bbgraph))
          next' = M.map (map f) (M.mapKeys f (next bbgraph))
          layout' = map f (layout bbgraph)
          args' = args bbgraph
          start' = f (start bbgraph)
          end' = map f (end bbgraph)


fixPhiInDest :: Int -> Int -> BBGraph -> BBGraph
fixPhiInDest s e bbgraph = bbgraph { ids = ids' }
    where ids' = M.map f (ids bbgraph)
          f bb = bb { bbCode = map f2 (bbCode bb) }
          f2 (IR_Phi x vs) = IR_Phi x (map (\(i, v) -> (if i == s then e else i, v)) vs)
          f2 ir = ir


mergeGraphs :: BBGraph -> BBGraph -> Int -> BBGraph
mergeGraphs gDest gSrc idx = G { ids = ids'', prev = prev'', next = next'', layout = layout', args = args', start = start', end = end' }
    where n = maximum (M.keys (ids gDest)) + 1
          gSrc' = renameIds (+n) gSrc
          s = start gSrc'
          e = head (end gSrc')
          gDest' = fixPhiInDest idx e gDest
          ids' = M.union (ids gDest') (ids gSrc')
          idxLabel = bbLabel ((ids gDest') M.! idx)
          sLabel = bbLabel ((ids gSrc') M.! s)
          f label = if label == idxLabel then sLabel else label
          ids'' = M.map (\bb -> bb { bbCode = map (modifyLabel f) (bbCode bb)}) ids'
          prev' = M.insert idx [] $ M.insert s ((prev gDest') M.! idx) $ (M.union (prev gDest') (prev gSrc'))
          prev'' = foldl (\m v -> M.adjust (\v' -> map (\x -> if x == idx then e else x) v') v m) prev' ((next gDest') M.! idx)
          next' = foldl (\m v -> M.insert v ((next gDest') M.! idx) m) (M.union (next gDest') (next gSrc')) (end gSrc')
          next'' = foldl (\m v -> M.adjust ((s:) . (filter (not . (==idx)))) v m) next' ((prev gDest') M.! idx)
          layout' = concat (map (\v -> if v == idx then layout gSrc' else [v]) (layout gDest'))
          args' = args gDest'
          start' = if idx == start gDest' then start gSrc' else start gDest'
          end' = if elem idx (end gDest') then (end gSrc') ++ (end gDest') else end gDest'
