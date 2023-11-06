module Execute
  ( execute,
  )
where


import Compile

type ProgramState = [Stack]

execute :: [Instruction] -> ProgramState -> Int -> ProgramState

execute _ state 0 = state
execute compiled state count =
  let (resState, didExecute) = executeInstructions compiled state
  in if didExecute then execute compiled resState (count - 1) else resState

canExecuteInstruction :: Instruction -> ProgramState -> Bool
--canExecuteInstruction (cwds,_) state = and $ map (\(a,b) -> a==b) $ zip cwds state
canExecuteInstruction (cwds,_) state = and $ map (uncurry stackCanBeRemoved) $ zipOrDefault [] [] cwds state

stackCanBeRemoved :: Stack -> Stack -> Bool
--stackCanBeRemoved stackToBeRemoved stack
stackCanBeRemoved (x:stackToBeRemoved) (y:stack) = x == y && stackCanBeRemoved stackToBeRemoved stack
stackCanBeRemoved [] _ = True
stackCanBeRemoved _ _ = False

executeInstructions :: [Instruction] -> ProgramState -> (ProgramState, Bool)
executeInstructions [] state = (state, False)
executeInstructions (instruction:rest) state = 
  let canExecute = canExecuteInstruction instruction state
      (changedState, didExecute) = if canExecute then 
          (modifyState instruction state, True)
        else 
          (state, False) 
      (resState, resDidExecute) = executeInstructions rest changedState
  in (resState, didExecute || resDidExecute)
      

modifyState :: Instruction -> ProgramState -> ProgramState
modifyState instruction state = addInstruction instruction $ removeInstruction instruction state

addInstruction :: Instruction -> ProgramState -> ProgramState
addInstruction (_, add) state = 
  map (\(ins, stack) -> ins ++ stack) $ zipOrDefault [] [] add state

removeInstruction :: Instruction -> ProgramState -> ProgramState
removeInstruction (cwds,_) state = map (\(remove, stack) -> removeFrom remove stack) $ zipOrDefault [] [] cwds state

removeFrom :: [a] -> [b] -> [b]
removeFrom (_:a) (_:ls) = removeFrom a ls
removeFrom [] ls = ls

zipOrDefault :: a -> b -> [a] -> [b] -> [(a,b)]
zipOrDefault aDef bDef (a:aRest) (b:bRest) = (a, b):zipOrDefault aDef bDef aRest bRest 
zipOrDefault aDef bDef [] (b:bRest) = (aDef, b):zipOrDefault aDef bDef [] bRest 
zipOrDefault aDef bDef (a:aRest) [] = (a, bDef):zipOrDefault aDef bDef aRest []
zipOrDefault aDef bDef [] [] = []
