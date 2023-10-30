module Main (main) where

import System.Environment   
import System.Exit
import System.IO
import Compile (compile, compileMultipleStacks, Compiled, Stack)
import ToString (compiledToString, stacksToString)
import Execute (execute)
import ParseInt (parsePosInt)

code :: String
code =
  "0|1:0|101\n\
  \:0"

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e

main :: IO ()
main = do
  let documentation = "Usage: flanck <file-path> [starting-stacks] [max-execution-count]\n\n\
  \"
  let error = "Error: need file path\n\n"
  args <- getArgs
  if null args
    then exitWithErrorMessage error (ExitFailure 2)
    else do
      handle <- openFile (head args) ReadMode
      content <- hGetContents handle
      let startingStacks = if length args >= 2 then compileMultipleStacks $ args !! 1 else []
      let maxExecutions = if length args >= 3 then parsePosInt (args !! 2) (100000000) else 1000000000
      let compiled = compile content 
      let res = execute compiled startingStacks maxExecutions
      putStrLn $ stacksToString res
      hClose handle
