
module Main where

import Jenseits
import Lexer
import Parser
import qualified Ast as A
import Translate
import Names
import Scope
import Canon
import Backend
import X86CodeGen
import InterferGraph -- for test purposes
import RegAlloc

import System.IO
import System.Exit(exitSuccess, exitFailure)
import System.FilePath
import System.Environment (getArgs)
import Control.Exception(catch)
import qualified Data.Vector as VB

main :: IO ()
main = do
   args <- System.Environment.getArgs
   case args of
     [('-':_)]        -> showUsage
     [fileName]       -> doMain False fileName
     ["-v", fileName] -> doMain True fileName
     _ ->
       showUsage
  where
    showUsage = do
       putStrLn ("usage: straigtline-exe -v <mini-java file>")
       putStrLn " -v: output intermediate results"
       exitFailure
       
doMain :: Bool -> String -> IO()
doMain verboseMode fileName = do
 result <- catch (if verboseMode then
                    processVerbose fileName >> return ""
                  else
                    process fileName >> return "")
             (\(ApplicationException errTxt) -> return errTxt)
 case result of
   [] -> exitSuccess
   errTxt -> do
     putStrLn $ "\nparsing of \"" ++ fileName ++ "\" failed:\n" ++
                errTxt ++ "\n"
     exitFailure
       
process :: String -> IO ()
process fileName = do
  input <- readFile fileName
  
  let sFileName = (takeFileName $ replaceExtension fileName "s")
  writeFile sFileName (show . runNameGen $
      return (parse . alexScanTokensMJ $ input)
      >>= translate
      >>= canPrg             -- Kanonisierung
      >>= codeGen X86CodeGen -- Kachelung
      >>= regAlloc (allRegisters X86CodeGen) (generalPurposeRegisters X86CodeGen)
      )

processVerbose :: String -> IO ()
processVerbose fileName = do
  input <- readFile fileName
  let astPrg = parse . alexScanTokens $ input
--  putStrLn (show astPrg)
  outh <-openFile "scope.txt" WriteMode
  listScopes outh astPrg
  hClose outh
  let treeFileName = (takeFileName $ replaceExtension fileName "tree")
  writeFile treeFileName $ show . runNameGen $ translate astPrg >>= canPrg

  regAllocDump  $ runNameGen $
    translate astPrg >>= canPrg >>= codeGen X86CodeGen
  
  let sFileName = (takeFileName $ replaceExtension fileName "s")
  writeFile sFileName (show . runNameGen $
      translate astPrg
      >>= canPrg             -- Kanonisierung
      >>= codeGen X86CodeGen -- Kachelung
      >>= regAlloc (allRegisters X86CodeGen) (generalPurposeRegisters X86CodeGen)
      )

  
listScopes :: Handle-> A.Prg -> IO()
listScopes outh (A.Prg _s classDeclS) = do
  let scMain = scope_InitMain classDeclS
  hPutStr outh $ scope_show scMain
  mapM_ (printMetScope scMain)
    [(clsName, varDeclS, metDecl) |
     (A.ClassDecl clsName _ varDeclS metDeclS) <- classDeclS,
     metDecl <- metDeclS]
  where
    printMetScope :: Scope -> (String, [A.VarDecl], A.MetDecl) -> IO()
    printMetScope scMain (clsName, varDeclS, metDecl) = do
      hPutStr outh ("\n" ++ (scope_show $
                    runNameGen $ scope_InitMet scMain clsName varDeclS metDecl))

-- ****************************************************************************
regAllocDump ::
    (Show i, MachineInstr i, MachineFunction f i, MachinePrg p f i) => p -> IO()
regAllocDump prg = mapM_ dumpFunc (machinePrgFunctions prg)
  where
    dumpFunc :: (Show i, MachineInstr i, MachineFunction f i) => f -> IO()
    dumpFunc func = do
      let instrs = VB.fromList $ machineFunctionBody func
      let (ifg, tempToTempNo, (cfg,liveOuts)) = interferGraph instrs

      writelnFileDL (name ++ "_cfg.dot") []
            (cfgShow cfg instrs liveOuts $ mkTempNoToTemp tempToTempNo)
        >>= putStrLn . ("cfg file written: " ++)

      writelnFileDL (name ++ "_ifg.dot") []
            (ifgShow ifg $ mkTempNoToTemp tempToTempNo)
        >>= putStrLn . ("ifg file written: " ++)

      where
        name = machineFunctionName func
