module Main where

import System.IO
import System.FilePath
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.Process

import Control.Monad ( liftM, when )

import qualified Data.Map as M
import qualified Data.Set as S

import LexSPL
import ParSPL
import PrintSPL
import AbsSPL

import ArgParse
import CalculateLiveVars
import CodeM
import FinishOptimizations
import GenerateIR ( runGenerateIR )
import GenCode ( genCode )
import GraphColoring ( colorBBGraph )
import ErrM
import IR
import BasicBlock
import Optimizations
import StaticCheck
import SSA

type Parser = [Token] -> Err (Program Pos)

parser :: Parser
parser = parseProgram

lexer :: String -> [Token]
lexer = myLexer

removeExtension :: FilePath -> FilePath
removeExtension filepath =
    let dir = takeDirectory filepath
        basename = takeBaseName filepath
     in dir ++ "/" ++ basename

changeExtension :: FilePath -> String -> FilePath
changeExtension filepath ext =
    let dir = takeDirectory filepath
        basename = takeBaseName filepath
     in dir ++ "/" ++ basename ++ "." ++ ext

writeIR_BBGraph :: Handle -> BBGraph -> IO ()
writeIR_BBGraph h g = do
    let liveVars = calculateLiveVars g
    let t = "\t\t\t\t\t\t\t\t"
    let bbs = flattenBBGraph g
    let m ir v = hPutStrLn h ("\t" ++ show ir ++ "\n" ++ t ++ show (S.toList v))
    let showBB (BB name xs) vars = do
                              hPutStrLn h (show name ++ ":")
                              hPutStrLn h (t ++ show (S.toList (head vars)))
                              mapM_ (uncurry m) (zip xs (tail vars))
    let showI i = showBB ((ids g) M.! i) (liveVars M.! i)
    mapM_ showI bbs


writeIR :: Handle -> (VIdent, BBGraph) -> IO ()
writeIR h (fName, g) = do
    let (g', regs) = colorBBGraph g
    hPutStrLn h (show fName ++ ":")
    writeIR_BBGraph h g'
    hPutStrLn h "register allocations:\n"
    let showR (x, r) = hPutStrLn h (show x ++ " -> " ++ show r)
    mapM_ showR (M.assocs regs)
    hPutStrLn h "\n\n"

writeCodeMap :: Handle -> M.Map VIdent [Code] -> IO ()
writeCodeMap h codeMap = do
    hPutStrLn h "; Generated by SPL compiler\n"
    hPutStrLn h "\textern printInt"
    hPutStrLn h "\textern allocMemory"
    hPutStrLn h "\textern freeMemory"
    hPutStrLn h ""
    hPutStrLn h "\tsection .text\n"
    mapM_ (writeCode h) (M.assocs codeMap)

writeCode :: Handle -> (VIdent, [Code]) -> IO ()
writeCode h (fName, xs) = do
    hPutStrLn h ("; " ++ show fName ++ "\n")
    hPutStrLn h ("\tglobal " ++ show fName ++ "\n")
    hPutStrLn h (show fName ++ ":")
    let f (CLabel l) = hPutStrLn h (show l ++ ":")
        f c = hPutStrLn h ("\t" ++ show c)
    mapM_ f xs
    hPutStrLn h ""

compileProgram :: String -> Err (Program (), M.Map VIdent BBGraph,  M.Map VIdent [Code])
compileProgram fileContent = do
    let abstractTree = lexer fileContent
    program <- parser abstractTree
    program' <- staticCheck program
    code <- runGenerateIR program'
    ir <- optimizeCode code
    let code = M.map genCode ir
    return (fmap (const ()) program, ir, code)


optimizeCode :: M.Map VIdent [IR] -> Err (M.Map VIdent BBGraph)
optimizeCode p = do
    let p' = M.mapWithKey splitIntoBasicBlocks p
    p <- mapM toSSA p'
    p <- return (basicOptimize p)
    p <- return (optimize p)
    p <- return (M.map finishOptimizations p)
    return p

run :: Bool -> Bool -> String -> IO ()
run bShowTree bSaveIR filepath = do
    hPutStrLn stderr ("Compiling " ++ filepath)
    fileContent <- readFile filepath
    case compileProgram fileContent of
      Bad errorMsg -> do
          hPutStrLn stderr errorMsg
          exitFailure
      Ok (program, ir, code) -> do
          when bShowTree (hPutStrLn stdout (printTree program))
          let fileIR = changeExtension filepath "ir"
          let fileCode = changeExtension filepath "s"
          let fileObj = changeExtension filepath "o"
          let fileOut = removeExtension filepath
          when bSaveIR (do
              handleIR <- openFile fileIR WriteMode
              mapM_ (writeIR handleIR) (M.assocs ir)
              hClose handleIR)
          handleCode <- openFile fileCode WriteMode
          writeCodeMap handleCode code
          hClose handleCode
          callCommand ("nasm -felf64 -o " ++ fileObj ++ " " ++ fileCode)
          callCommand ("gcc -no-pie -o " ++ fileOut ++ " " ++ fileObj ++ " lib/runtime.o")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
        hPutStrLn stderr "Usage: ./spl source-files"
        exitFailure
    _ -> do
        let a = parseArgs args
        let aShowTree = ArgParse.showTree a
        let bSaveIR = saveIR a
        let aFilePaths = filepaths a
        mapM_ (run aShowTree bSaveIR) aFilePaths
