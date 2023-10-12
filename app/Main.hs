module Main (main) where

import Compile (compile)
import ToString (compiledToString)

code :: String
code =
  "010011|101:101010|1\n\
  \1001:1000"

main :: IO ()
main = print $ compiledToString $ compile code