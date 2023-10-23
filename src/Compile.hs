{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Compile
  ( compile,
    Compiled,
    Instruction,
    Stack,
  )
where

type Stack = [Bool]

type Instruction = ([Stack], [Stack])

type Compiled = [Instruction]

type StatementParser = [Char] -> Compiled -> Compiled

compile :: [Char] -> Compiled
compile src = reverseAllDefinitions (beginStatement src [])

reverseAllDefinitions :: Compiled -> Compiled
reverseAllDefinitions c = reverse [(reverse [reverse cbd | cbd <- allcbds], reverse [reverse wbd | wbd <- allwbds]) | (allcbds, allwbds) <- c]

beginStatement :: StatementParser
beginStatement src c = cbds src (([[]], [[]]) : c)

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