module Main where

import System.IO ( stdout, stdin, stderr, hGetContents, hPutStrLn )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Data.Map as Map

import LexSPL
import ParSPL
import SkelSPL
import PrintSPL
import AbsSPL

import CodeM
import Defs ( Pos )
import ExprTypeCheck ( typeProgram )
import GenerateIR ( runGenerateIR )
import GenCode ( genCode )
import ErrM
import IR
import BasicBlock
import Optimizations
import StaticCheck
import SSA

type ParseFun a = [Token] -> Err a

myLLexer :: String -> [Token]
myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

printBasicBlock :: BasicBlock -> IO ()
printBasicBlock (BB name code) = do
    hPutStrLn stdout (show name)
    mapM_ (\ir -> hPutStrLn stdout ("\t" ++ show ir)) code
    hPutStrLn stdout ""

printBBGraph :: (Ident, BBGraph) -> IO ()
printBBGraph (name, g) = do
    hPutStrLn stdout (show name ++ ":")
    hPutStrLn stdout "IDS:"
    mapM_ (\(i, BB name _) -> hPutStrLn stdout ("\t" ++ show i ++ " -> " ++ show name)) (Map.toList (ids g))
    hPutStrLn stdout "NEXT:"
    mapM_ (\(n, ns) -> hPutStrLn stdout ("\t" ++ show n ++ " -> " ++ show ns)) (Map.toList (next g))
    hPutStrLn stdout "PREV:"
    mapM_ (\(n, ns) -> hPutStrLn stdout ("\t" ++ show n ++ " -> " ++ show ns)) (Map.toList (next g))
    hPutStrLn stdout "BASIC BLOCKS:"
    mapM_ printBasicBlock (Map.elems (ids g))

printCode :: (Ident, [Code]) -> IO ()
printCode (name, xs) = do
    hPutStrLn stdout (show name ++ ":")
    mapM_ (\x -> hPutStrLn stdout ("\t" ++ show x)) xs



compileProgram :: ParseFun (Program ()) -> String -> Err (Map Ident BBGraph)
compileProgram parser fileContent = do
    let abstractTree = myLLexer fileContent
    program <- parser abstractTree
    staticCheck program
    program <- typeProgram program
    code <- runGenerateIR program
    let bbgraphs = Map.map optimizeCode code
    let code = Map.map (genCode . layoutBBGraph) bbgraphs
    return bbgraphs

optimizeCode :: [IR] -> BBGraph
optimizeCode =  removePhi .
                optimize .
                toSSA .
                splitIntoBasicBlocks

run :: ParseFun (Program ()) -> String -> IO ()
run parser filepath = do
    fileContent <- readFile filepath
    let abstractTree = myLLexer fileContent
    case compileProgram parser fileContent of
      Bad errorMsg -> do
          hPutStrLn stderr errorMsg
          exitFailure
      Ok code -> do
          mapM_ printBBGraph (Map.assocs code)
          exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
        hPutStrLn stderr "Usage: ./spl source-files"
        exitFailure
    filepaths -> mapM_ (run pProgram) filepaths
