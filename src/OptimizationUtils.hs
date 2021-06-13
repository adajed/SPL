module OptimizationUtils where

import qualified Data.Map as M

import BasicBlock
import IR
import Token ( VIdent(..) )

type Ind a = (Int, a)

modifyValue :: (ValIR -> ValIR) -> IR -> IR
modifyValue f ir =
    case ir of
      IR_Label l -> IR_Label l
      IR_Ass x v -> IR_Ass x (f v)
      IR_BinOp op x v1 v2 -> IR_BinOp op x (f v1) (f v2)
      IR_UnOp op x v -> IR_UnOp op x (f v)
      IR_MemRead x v -> IR_MemRead x (f v)
      IR_MemSave v1 v2 size -> IR_MemSave (f v1) (f v2) size
      IR_Call x v vs -> IR_Call x (f v) (fmap f vs)
      IR_VoidCall v vs -> IR_VoidCall (f v) (fmap f vs)
      IR_Return v -> IR_Return (f v)
      IR_VoidReturn -> IR_VoidReturn
      IR_Jump l -> IR_Jump l
      IR_CondJump v1 op v2 l1 l2 -> IR_CondJump (f v1) op (f v2) l1 l2
      IR_Phi x vs -> IR_Phi x (map (\(n,v) -> (n, f v)) vs)

modifyVar :: (SVar -> SVar) -> IR -> IR
modifyVar f ir =
    case ir of
      IR_Ass x v -> IR_Ass (f x) v
      IR_BinOp op x v1 v2 -> IR_BinOp op (f x) v1 v2
      IR_UnOp op x v -> IR_UnOp op (f x) v
      IR_MemRead x v -> IR_MemRead (f x) v
      IR_Call x v vs -> IR_Call (f x) v vs
      IR_Phi x vs -> IR_Phi (f x) vs
      ir' -> ir'

modifyLabel :: (VIdent -> VIdent) -> IR -> IR
modifyLabel f ir =
    case ir of
      IR_Label label -> IR_Label (f label)
      IR_Jump label -> IR_Jump (f label)
      IR_CondJump v1 op v2 label1 label2 -> IR_CondJump v1 op v2 (f label1) (f label2)
      ir' -> ir'

litfToSVar :: (Var -> Var) -> SVar -> SVar
litfToSVar f (SVar var size) = SVar (f var) size

liftToValIR :: (Var -> Var) -> ValIR -> ValIR
liftToValIR f (VarIR sVar) = VarIR (litfToSVar f sVar)
liftToValIR f val = val

takeVar :: IR -> Maybe SVar
takeVar (IR_Ass x _) = Just x
takeVar (IR_BinOp _ x _ _) = Just x
takeVar (IR_UnOp _ x _) = Just x
takeVar (IR_MemRead x _) = Just x
takeVar (IR_Call x _ _) = Just x
takeVar (IR_Phi x _) = Just x
takeVar _ = Nothing

maybeVar :: ValIR -> [SVar]
maybeVar (VarIR x) = [x]
maybeVar _ = []

getAllVars :: IR -> [SVar]
getAllVars (IR_Label _) = []
getAllVars (IR_Ass x v) = x:(maybeVar v)
getAllVars (IR_BinOp _ x v1 v2) = x:(concat (map maybeVar [v1, v2]))
getAllVars (IR_UnOp _ x v) = x:(maybeVar v)
getAllVars (IR_MemRead x v) = x:(maybeVar v)
getAllVars (IR_MemSave v1 v2 _) = concat (map maybeVar [v1, v2])
getAllVars (IR_Call y f xs) = [y] ++ maybeVar f ++ concat (map maybeVar xs)
getAllVars (IR_VoidCall f xs) = maybeVar f ++ concat (map maybeVar xs)
getAllVars (IR_Return v) = maybeVar v
getAllVars (IR_VoidReturn) = []
getAllVars (IR_Jump _) = []
getAllVars (IR_CondJump v1 _ v2 _ _) = maybeVar v1 ++ maybeVar v2
getAllVars (IR_Phi x vs) = x:(concat (map (maybeVar . snd) vs))
getAllVars (IR_Nop) = []
getAllVars (IR_Argument x) = []
getAllVars (IR_Store x) = [x]
getAllVars (IR_Load x) = [x]

mapIR :: (IR -> IR) -> BBGraph -> BBGraph
mapIR f g = g { ids = M.map (\(BB name xs) -> BB name (map f xs)) (ids g) }

liftBB :: ([IR] -> [IR]) -> BasicBlock -> BasicBlock
liftBB f (BB name xs) = BB name (f xs)

change :: Int -> a -> [a] -> [a]
change n x xs = (take n xs) ++ [x] ++ (drop (n+1) xs)

changeBBGraph :: Int -> Int -> IR -> BBGraph -> BBGraph
changeBBGraph i n ir g = g { ids = ids' }
    where ids' = M.adjust f i (ids g)
          f (BB name xs) = BB name (change n ir xs)

allPairs :: [a] -> [(Ind a, Ind a)]
allPairs xs = h (zip [0..] xs) []
    where h :: [Ind a] -> [(Ind a, Ind a)] -> [(Ind a, Ind a)]
          h [] acc = acc
          h (y:ys) acc = h ys ((zip (repeat y) ys) ++ acc)

allTriples :: [a] -> [(Ind a, Ind a, Ind a)]
allTriples xs = h1 (zip [0..] xs) []
    where h1 :: [Ind a] -> [(Ind a, Ind a, Ind a)] -> [(Ind a, Ind a, Ind a)]
          h1 [] acc = acc
          h1 (y:ys) acc = h1 ys ((zip' (repeat y) ys') ++ acc)
              where ys' = h2 ys []
          h2 :: [Ind a] -> [(Ind a, Ind a)] -> [(Ind a, Ind a)]
          h2 [] acc = acc
          h2 (y:ys) acc = h2 ys ((zip (repeat y) ys) ++ acc)
          zip' = zipWith (\a -> \(b, c) -> (a, b, c))
