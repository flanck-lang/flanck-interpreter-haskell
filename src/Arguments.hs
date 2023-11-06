module Arguments (removeAllArguments, hasArgument) where

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith (a:lsa) (b:lsb)
  | a == b = startsWith lsa lsb
  | otherwise = False
startsWith [] _ = True
startsWith _ _ = False

hasArgument :: String -> [String] -> Bool
hasArgument argument ls = any (startsWith $ "--" ++ argument) ls

removeAllArguments :: [String] -> [String]
removeAllArguments = filter $ not . isArgument

isArgument :: String -> Bool
isArgument ('-':'-':_) = True
isArgument _ = False