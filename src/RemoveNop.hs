module RemoveNop where

import Data.Map as Map

import BasicBlock
import IR

removeNop :: BBGraph -> BBGraph
removeNop g = g { ids = Map.map removeNopBB (ids g) }
    where removeNopBB (BB name xs) = BB name (Prelude.filter ((/=) IR_Nop) xs)
