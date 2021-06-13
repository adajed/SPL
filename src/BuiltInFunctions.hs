module BuiltInFunctions where

import Token ( VIdent(..) )

builtInFunctions :: [VIdent]
builtInFunctions = map VIdent ["printInt", "print", "freeMemory", "allocMemory"]
