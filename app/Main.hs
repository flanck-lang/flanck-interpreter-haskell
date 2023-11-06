module Main (main) where

import System.Environment   
import System.Exit
import System.IO

import Data.Maybe


import Compile (compile, compileMultipleStacks, Compiled, Stack)
import ToString (compiledToString, stacksToString)
import Execute (execute)
import ParseInt (parsePosInt)
import Arguments (removeAllArguments, hasArgument)
import ASCII (stringToBooleans, booleansToString)

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e

-- stack 0: input
-- stack 1: output

val :: Maybe a -> a
val (Just x) = x

main :: IO ()
main = do
  let documentation = "Usage: flanck <file-path> [input] [max-execution-count]\n\n\
  \Options:\n\
  \  --output-stacks-b     If to output stacks binary or the second\
  \                        stack as ascii (default: ascii).\n\n\
  \  --old-output-stacks   If the --output-stacks-b option is enabled,\
  \                        if to output the stacks in the classic syntax (default: false).\n\n\
  \  --input-stacks-b      If to input stacks binary\
  \                        (pipe symbol for multiple stacks starting at index 0)\
  \                        or input the second stack with the input interpreted\
  \                        in ascii (default: ascii).\n\n\
  \  --help                Show this help information.\n\
  \"
  let error = "Error: need file path\n\n"
  rawArgs <- getArgs
  let args = removeAllArguments rawArgs
  let outputStacks = hasArgument "output-stacks-b" rawArgs
  let oldOutputStacks = hasArgument "old-output-stacks" rawArgs
  let inputStacks = hasArgument "input-stacks-b" rawArgs
  let help = hasArgument "help" rawArgs
  if help then do putStrLn documentation
  else do 
    if null args
      then exitWithErrorMessage error (ExitFailure 2)
      else do
        handle <- openFile (head args) ReadMode
        content <- hGetContents handle
        let input = if length args >= 2 then Just $ args !! 1 else Nothing
        let startingStacks = if isNothing input 
            then [[True]] 
            else if inputStacks
              then compileMultipleStacks $ val input
              else [stringToBooleans $ val input]
        let maxExecutions = if length args >= 3 then parsePosInt (args !! 2) (100000000) else 1000000000
        let compiled = compile content 
        let res = execute compiled startingStacks maxExecutions
        if outputStacks
          then do
            putStrLn $ stacksToString res
          else do
            putStrLn $ booleansToString $ res !! 1 --last booleans cut off or first booleans?
        hClose handle
