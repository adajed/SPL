module CalculateLiveVars (
    BBLiveVars,
    calculateLiveVars,
    uses,
    kill
                         ) where

import qualified Data.Map as M
import qualified Data.Set as S

import IR
import BasicBlock

type BBLiveVars = M.Map Int [S.Set SVar]

calculateLiveVars :: BBGraph -> BBLiveVars
calculateLiveVars g = calculateLiveVars' initOuts
    where h (BB _ xs) = (S.empty):(map (const S.empty) xs)
          initOuts = M.map h (ids g)
          calculateLiveVars' outs =
              let newouts = calculateLive outs g
               in if outs == newouts
                     then outs
                     else calculateLiveVars' (combineOuts g newouts)

calculateIn :: S.Set SVar -> IR -> S.Set SVar
calculateIn out ir = S.union (S.difference out (kill ir)) (uses ir)

calculateLiveBB :: S.Set SVar -> BasicBlock -> [S.Set SVar]
calculateLiveBB out (BB name xs) = reverse (scanl calculateIn out (reverse xs))

calculateLive :: BBLiveVars -> BBGraph -> BBLiveVars
calculateLive m g = M.mapWithKey f m
    where f i out = calculateLiveBB (last out) ((ids g) M.! i)

combineOuts :: BBGraph -> BBLiveVars -> BBLiveVars
combineOuts g m = M.mapWithKey f m
    where f i outs = (init outs) ++ [newlast i]
          newlast i = S.unions (map (head . (m M.!)) ((next g) M.! i))

getVar :: ValIR -> S.Set SVar
getVar (VarIR var) = S.singleton var
getVar _ = S.empty

kill :: IR -> S.Set SVar
kill (IR_Ass x _) = S.singleton x
kill (IR_BinOp _ x _ _) = S.singleton x
kill (IR_UnOp _ x _) = S.singleton x
kill (IR_MemRead x _) = S.singleton x
kill (IR_Call x _ _) = S.singleton x
kill (IR_Load x) = S.singleton x
kill _ = S.empty

uses :: IR -> S.Set SVar
uses (IR_Ass _ v) = getVar v
uses (IR_BinOp _ _ v1 v2) = S.unions (map getVar [v1, v2])
uses (IR_UnOp _ _ v) = getVar v
uses (IR_MemRead _ v) = getVar v
uses (IR_MemSave v1 v2 _) = S.unions (map getVar [v1, v2])
uses (IR_Call _ v xs) = S.unions ((getVar v):(map getVar xs))
uses (IR_VoidCall v xs) = S.unions ((getVar v):(map getVar xs))
uses (IR_Return v) = getVar v
uses (IR_CondJump v1 _ v2 _) = S.unions (map getVar [v1, v2])
uses (IR_Store x) = S.singleton x
uses _ = S.empty

