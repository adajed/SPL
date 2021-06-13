module ArgParse where


import qualified Data.List as L
import System.IO
import System.Exit ( exitFailure, exitSuccess )

import Optimizations ( OptimizationOptions(..) )

data Args = Args { showTree :: Bool
                 , saveIR :: Bool
                 , optimizationOptions :: OptimizationOptions
                 , filepaths :: [String]
                 , noRegisterAllocation :: Bool
                 , debugBuild :: Bool
                 }

options0 :: OptimizationOptions
options0 = OptimizationOptions { doConstantFolding = False
                               , doArithmeticOptimizations = False
                               , doCopyPropagation = False
                               , doDeadCodeElimination = False
                               , doCommonSubexpressionElimination = False
                               , doRegisterAllocation = False
                               , doInlineFunctions = False
                               }

options1 :: OptimizationOptions
options1 = OptimizationOptions { doConstantFolding = True
                               , doArithmeticOptimizations = True
                               , doCopyPropagation = True
                               , doDeadCodeElimination = True
                               , doCommonSubexpressionElimination = False
                               , doRegisterAllocation = True
                               , doInlineFunctions = False
                               }
options2 :: OptimizationOptions
options2 = OptimizationOptions { doConstantFolding = True
                               , doArithmeticOptimizations = True
                               , doCopyPropagation = True
                               , doDeadCodeElimination = True
                               , doCommonSubexpressionElimination = True
                               , doRegisterAllocation = True
                               , doInlineFunctions = False
                               }

options3 :: OptimizationOptions
options3 = OptimizationOptions { doConstantFolding = True
                               , doArithmeticOptimizations = True
                               , doCopyPropagation = True
                               , doDeadCodeElimination = True
                               , doCommonSubexpressionElimination = True
                               , doRegisterAllocation = True
                               , doInlineFunctions = True
                               }

startArgs :: Args
startArgs = Args { showTree = False
                 , saveIR = False
                 , optimizationOptions = options2
                 , filepaths = []
                 , noRegisterAllocation = False
                 , debugBuild = False
                 }


override :: Bool -> (Args -> Args) -> Args -> Args
override b f = if b then f else id

parseArgsExit :: Args -> Args
parseArgsExit args = override (noRegisterAllocation args) f1 args
    where f1 a = a { optimizationOptions = (optimizationOptions a) { doRegisterAllocation = False } }

parseArgs :: [String] -> IO Args
parseArgs args = parseArgs' args startArgs
    where parseArgs' :: [String] -> Args -> IO Args
          parseArgs' [] args = return (parseArgsExit args)
          parseArgs' (x:xs) args = case x of
                                     "--help" -> do
                                         hPutStrLn stderr "Usage: ./spl [--show-tree] [--write-ir] source-files"
                                         exitSuccess
                                     "--show-tree" -> parseArgs' xs (args { showTree = True })
                                     "--write-ir" -> parseArgs' xs (args { saveIR = True })
                                     "-g" -> parseArgs' xs (args { debugBuild = True })
                                     "-O0" -> parseArgs' xs (args { optimizationOptions = options0 })
                                     "-O1" -> parseArgs' xs (args { optimizationOptions = options1 })
                                     "-O2" -> parseArgs' xs (args { optimizationOptions = options2 })
                                     "-O3" -> parseArgs' xs (args { optimizationOptions = options3 })
                                     "-Wno-register-allocation" -> parseArgs' xs ( args { noRegisterAllocation = True })
                                     _ -> if L.isPrefixOf "-" x
                                             then hPutStrLn stderr ("Error: unknown argument " ++ show x) >> exitFailure
                                             else parseArgs' xs (args { filepaths = x:(filepaths args) })
