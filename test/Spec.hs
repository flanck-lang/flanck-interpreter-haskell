import Test.Tasty
import Test.Tasty.SmallCheck

import Data.List
import Data.Ord

import ASCII

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ascii]

ascii :: TestTree
ascii = testGroup "ASCII" 
  [
    testProperty "char <-> word" $
      \c -> c == (wordToChar $ charToWord c),
    testProperty "trunicateBooleansToLastBytes" $
      trunicateBooleansToLastBytes [True, False] == [],
    testProperty "more trunicateBooleansToLastBytes" $
      trunicateBooleansToLastBytes [True, False, True, True, False, False, True, True, False] == [False, True, True, False, False, True, True, False],
    testProperty "booleans <-> strings" $
      \booleans -> (stringToBooleans $ booleansToString $ trunicateBooleansToLastBytes booleans) == trunicateBooleansToLastBytes booleans
  ]
