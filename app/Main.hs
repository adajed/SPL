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

import Defs ( Pos )
import ExprTypeCheck ( typeProgram )
import GenerateIR ( runGenerateIR )
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

printBBGraph :: BBGraph -> IO ()
printBBGraph g = do
    hPutStrLn stdout "IDS:"
    mapM_ (\(i, BB name _) -> hPutStrLn stdout ("\t" ++ show i ++ " -> " ++ show name)) (Map.toList (ids g))
    hPutStrLn stdout "NEXT:"
    mapM_ (\(n, ns) -> hPutStrLn stdout ("\t" ++ show n ++ " -> " ++ show ns)) (Map.toList (next g))
    hPutStrLn stdout "PREV:"
    mapM_ (\(n, ns) -> hPutStrLn stdout ("\t" ++ show n ++ " -> " ++ show ns)) (Map.toList (next g))
    hPutStrLn stdout "BASIC BLOCKS:"
    mapM_ printBasicBlock (Map.elems (ids g))


compileProgram :: ParseFun (Program ()) -> String -> Err (Map Ident [IR])
compileProgram parser fileContent = do
    let abstractTree = myLLexer fileContent
    program <- parser abstractTree
    staticCheck program
    program <- typeProgram program
    runGenerateIR program

run :: ParseFun (Program ()) -> String -> IO ()
run parser filepath = do
    fileContent <- readFile filepath
    let abstractTree = myLLexer fileContent
    case compileProgram parser fileContent of
      Bad errorMsg -> do
          hPutStrLn stderr errorMsg
          exitFailure
      Ok code -> do
          let bbgraphs = Map.map (toSSA . splitIntoBasicBlocks) code
          let bbgraphs' = Map.map optimize bbgraphs
          mapM_ printBBGraph bbgraphs'
          exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> run pProgram filepath
    _ -> do
        hPutStrLn stderr "ERROR"
        exitFailure
