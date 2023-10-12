module ToString
  ( compiledToString,
  )
where

import Compile
import Data.List (intercalate)

compiledToString :: Compiled -> [Char]
compiledToString instructions = intercalate "\n" $ map instructionToString instructions

instructionToString :: Instruction -> [Char]
instructionToString (cbds, wbds) = stacksToString cbds ++ ":" ++ stacksToString wbds

stacksToString :: [[Bool]] -> [Char]
stacksToString stacks = intercalate "|" $ map (map (\v -> if v then '1' else '0')) stacks