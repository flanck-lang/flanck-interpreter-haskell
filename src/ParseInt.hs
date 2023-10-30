module ParseInt
  ( parsePosInt,
  )
where


parsePosInt :: String -> Int -> Int
parsePosInt s def = parseIntR s 0 def

parseIntR :: String -> Int -> Int -> Int

parseIntR (c:s) int def = 
  let digit = charToInt c
  in if digit == -1 
      then def
      else parseIntR s (int * 10 + digit ) def

parseIntR "" n _ = n

charToInt :: Char -> Int
charToInt c
  | c == '0' = 0
  | c == '1' = 1
  | c == '2' = 2
  | c == '3' = 3
  | c == '4' = 4
  | c == '5' = 5
  | c == '6' = 6
  | c == '7' = 7
  | c == '8' = 8
  | c == '9' = 9
  | otherwise = -1
