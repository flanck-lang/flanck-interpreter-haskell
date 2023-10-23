module Execute
  ( execute,
  )
where


import Compile

type ProgramState = [Stack]

execute :: [Instruction] -> ProgramState -> ProgramState
execute compiled state =
  let (resState, didExecute) = executeInstructions compiled state
  in if didExecute then execute compiled resState else resState

canExecuteInstruction :: Instruction -> ProgramState -> Bool
--canExecuteInstruction (cwds,_) state = and $ map (\(a,b) -> a==b) $ zip cwds state
canExecuteInstruction (cwds,_) state = and $ map (\(lsa, lsb) -> and $ map (\(a,b) -> a==b) $ zip lsa lsb) $ zipOrDefault [] [] cwds state


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
modifyState instruction = removeInstruction instruction
------------------------ ^ addInstructions . removeInstruction instruction
--addInstructions :: Instruction 

removeInstruction :: Instruction -> ProgramState -> ProgramState
removeInstruction (cwds,_) state = map (\(remove, stack) -> removeFrom remove stack)$ zipOrDefault [] [] cwds state

removeFrom :: [a] -> [b] -> [b]
removeFrom (_:a) (_:ls) = ls
removeFrom [] ls = ls

zipOrDefault :: a -> b -> [a] -> [b] -> [(a,b)]
zipOrDefault aDef bDef (a:aRest) (b:bRest) = (a, b):zipOrDefault aDef bDef aRest bRest 
zipOrDefault aDef bDef [] (b:bRest) = (aDef, b):zipOrDefault aDef bDef [] bRest 
zipOrDefault aDef bDef (a:aRest) [] = (a, bDef):zipOrDefault aDef bDef aRest []
zipOrDefault aDef bDef [] [] = []

--addInstructions :: Instruction -> ProgramState -> ProgramState
