module ArgParse where

import System.IO
import System.Exit ( exitFailure, exitSuccess )

import Optimizations ( OptimizationOptions(..) )

data Args = Args { showTree :: Bool
                 , saveIR :: Bool
                 , optimizationOptions :: OptimizationOptions
                 , filepaths :: [String]
                 }

options0 :: OptimizationOptions
options0 = OptimizationOptions { doConstantFolding = False
                               , doArithmeticOptimizations = False
                               , doCopyPropagation = False
                               , doDeadCodeElimination = False
                               , doCommonSubexpressionElimination = False
                               }

options1 :: OptimizationOptions
options1 = OptimizationOptions { doConstantFolding = True
                               , doArithmeticOptimizations = True
                               , doCopyPropagation = True
                               , doDeadCodeElimination = True
                               , doCommonSubexpressionElimination = False
                               }
options2 :: OptimizationOptions
options2 = OptimizationOptions { doConstantFolding = True
                               , doArithmeticOptimizations = True
                               , doCopyPropagation = True
                               , doDeadCodeElimination = True
                               , doCommonSubexpressionElimination = True
                               }

options3 :: OptimizationOptions
options3 = OptimizationOptions { doConstantFolding = True
                               , doArithmeticOptimizations = True
                               , doCopyPropagation = True
                               , doDeadCodeElimination = True
                               , doCommonSubexpressionElimination = True
                               }

startArgs :: Args
startArgs = Args { showTree = False
                 , saveIR = False
                 , optimizationOptions = options2
                 , filepaths = [] }

parseArgs :: [String] -> IO Args
parseArgs args = parseArgs' args startArgs
    where parseArgs' :: [String] -> Args -> IO Args
          parseArgs' [] args = return args
          parseArgs' (x:xs) args = case x of
                                     "--help" -> do
                                         hPutStrLn stderr "Usage: ./spl [--show-tree] [--write-ir] source-files"
                                         exitSuccess
                                     "--show-tree" -> parseArgs' xs (args { showTree = True })
                                     "--write-ir" -> parseArgs' xs (args { saveIR = True })
                                     "-O0" -> parseArgs' xs (args { optimizationOptions = options0 })
                                     "-O1" -> parseArgs' xs (args { optimizationOptions = options1 })
                                     "-O2" -> parseArgs' xs (args { optimizationOptions = options2 })
                                     "-O3" -> parseArgs' xs (args { optimizationOptions = options3 })
                                     _ -> parseArgs' xs (args { filepaths = x:(filepaths args) })
