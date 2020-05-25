module CalculateLiveVars where

import Data.Map as Map
import Data.Set as Set

import IR
import BasicBlock


calculateLiveVars :: BBGraph -> Map Int [Set SVar]
calculateLiveVars g = calculateLiveVars' initOuts
    where h (BB _ xs) = (Set.empty):(Prelude.map (const Set.empty) xs)
          initOuts = Map.map h (ids g)
          calculateLiveVars' outs =
              let newouts = calculateLive outs g
               in if outs == newouts
                     then outs
                     else calculateLiveVars' (combineOuts g newouts)

calculateIn :: Set SVar -> IR -> Set SVar
calculateIn out ir = Set.union (Set.difference out (kill ir)) (uses ir)

calculateLiveBB :: Set SVar -> BasicBlock -> [Set SVar]
calculateLiveBB out (BB name xs) = reverse (scanl calculateIn out (reverse xs))

calculateLive :: Map Int [Set SVar] -> BBGraph -> Map Int [Set SVar]
calculateLive m g = Map.mapWithKey f m
    where f i out = calculateLiveBB (last out) ((ids g) ! i)

combineOuts :: BBGraph -> Map Int [Set SVar] -> Map Int [Set SVar]
combineOuts g m = Map.mapWithKey f m
    where f i outs = (init outs) ++ [newlast i]
          newlast i = Set.unions (Prelude.map (head . (m!)) ((next g) ! i))

getVar :: ValIR -> Set SVar
getVar (VarIR var) = Set.singleton var
getVar _ = Set.empty

kill :: IR -> Set SVar
kill (IR_Ass x _) = Set.singleton x
kill (IR_BinOp _ x _ _) = Set.singleton x
kill (IR_UnOp _ x _) = Set.singleton x
kill (IR_MemRead x _) = Set.singleton x
kill (IR_Call x _ _) = Set.singleton x
kill _ = Set.empty

uses :: IR -> Set SVar
uses (IR_Ass _ v) = getVar v
uses (IR_BinOp _ _ v1 v2) = Set.unions (Prelude.map getVar [v1, v2])
uses (IR_UnOp _ _ v) = getVar v
uses (IR_MemRead _ v) = getVar v
uses (IR_MemSave v1 v2 _) = Set.unions (Prelude.map getVar [v1, v2])
uses (IR_Call _ v xs) = Set.unions ((getVar v):(Prelude.map getVar xs))
uses (IR_VoidCall v xs) = Set.unions ((getVar v):(Prelude.map getVar xs))
uses (IR_Return v) = getVar v
uses (IR_CondJump v1 _ v2 _) = Set.unions (Prelude.map getVar [v1, v2])
uses _ = Set.empty

