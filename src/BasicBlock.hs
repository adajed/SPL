module BasicBlock where

import Data.Map as Map

import AbsSPL
import IR

data BasicBlock = BB Ident [IR]

splitIntoBasicBlocks :: Map Ident [IR] -> Map Ident [BasicBlock]
splitIntoBasicBlocks m = Map.map (help []) m
    where help bbs [] = reverse bbs
          help bbs xs = help ((BB name bb):bbs) xs'
            where (name, bb, xs') = getBasicBlock xs


getBasicBlock :: [IR] -> (Ident, [IR], [IR])
getBasicBlock ((IR_Label name):xs) = (name, reverse bb, xs')
    where (bb, xs') = getBasicBlock' xs []
getBasicBlock xs = (Ident "temp", reverse bb, xs')
    where (bb, xs') = getBasicBlock' xs []

getBasicBlock' :: [IR] -> [IR] -> ([IR], [IR])
getBasicBlock' ((IR_Jump l):xs) ys = (((IR_Jump l):ys), xs)
getBasicBlock' ((IR_CondJump v l):xs) ys = (((IR_CondJump v l):ys), xs)
getBasicBlock' ((IR_Return v):xs) ys = (((IR_Return v):ys), xs)
getBasicBlock' ((IR_Label l):xs) ys = (ys, (IR_Label l):xs)
getBasicBlock' (ir:xs) ys = getBasicBlock' xs (ir:ys)
