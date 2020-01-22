module BasicBlock where

import AbsSPL
import IR

data BasicBlock = BB Ident [IR]

splitIntoBasicBlocks :: [IR] -> [BasicBlock]
splitIntoBasicBlocks code = splitIntoBasicBlocks' code []
    where splitIntoBasicBlocks' [] bbs = reverse bbs
          splitIntoBasicBlocks' code bbs = splitIntoBasicBlocks' code' ((BB name bb):bbs)
            where (name, bb, code') = getBasicBlock code

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
