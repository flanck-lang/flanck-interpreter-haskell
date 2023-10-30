{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Compile
  ( compile,
    compileStack,
    compileMultipleStacks,
    Compiled,
    Instruction,
    Stack,
  )
where

type Stack = [Bool]

type Instruction = ([Stack], [Stack])

type Compiled = [Instruction]

type StatementParser = [Char] -> Compiled -> Compiled

split :: (Eq a) => a -> [a] -> [[a]]
-- filter (not . null) $ 
split e ls = map reverse $ reverse $ rSplit e ls [[]]
rSplit :: (Eq a) => a -> [a] -> [[a]] -> [[a]]
rSplit e (a:ls) (ar:r) = if e == a then rSplit e ls $ []:ar:r else rSplit e ls $ (a:ar):r
rSplit _ [] r = r


compile :: String -> Compiled
compile src = reverseAllDefinitions (beginStatement src [])

compileStack :: String -> Stack
compileStack = map (\x -> case x of '0' -> False; '1' -> True) . filter (\x -> x == '0' || x == '1')

compileMultipleStacks :: String -> [Stack]
compileMultipleStacks = map compileStack . split '|'

reverseAllDefinitions :: Compiled -> Compiled
reverseAllDefinitions c = reverse [(reverse [reverse cbd | cbd <- allcbds], reverse [reverse wbd | wbd <- allwbds]) | (allcbds, allwbds) <- c]

beginStatement :: StatementParser
beginStatement src c = cbds src $ ([[]], [[]]):c 

cbds :: StatementParser
cbds ('0' : src) ((cbd : rest, allwbds) : instructions) = cbds src (((False : cbd) : rest, allwbds) : instructions)
cbds ('1' : src) ((cbd : rest, allwbds) : instructions) = cbds src (((True : cbd) : rest, allwbds) : instructions)
cbds ('|' : src) ((allcbds, allwbds) : instructions) = cbds src (([] : allcbds, allwbds) : instructions)
cbds (':' : src) instructions = wbds src instructions
cbds ('\n' : src) (_ : instructions) = beginStatement src instructions
cbds [] (_ : instructions) = instructions
cbds (_ : src) instructions = cbds src instructions

wbds :: StatementParser
wbds ('0' : src) ((allcbds, wbd : rest) : instructions) = wbds src ((allcbds, (False : wbd) : rest) : instructions)
wbds ('1' : src) ((allcbds, wbd : rest) : instructions) = wbds src ((allcbds, (True : wbd) : rest) : instructions)
wbds ('|' : src) ((allcbds, allwbds) : instructions) = wbds src ((allcbds, [] : allwbds) : instructions)
wbds ('\n' : src) instructions = beginStatement src instructions
wbds [] instructions = instructions
wbds (_ : src) instructions = wbds src instructions

--TODO: at the end because of the list of stacks being reversed, the stacks dont really match up?