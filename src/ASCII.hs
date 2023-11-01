module ASCII (stringToWord8List, booleansToString, booleansToWord8List) where

import Data.Word
import Data.Bits

import qualified Data.ByteString.Char8 as C

stringToWord8List :: String -> [Word8]
stringToWord8List str = map charToWord $ C.unpack (C.pack str)

stringToBooleans :: String -> [Bool]
stringToBooleans = concat . map wordToBooleans . stringToWord8List

charToWord :: Char -> Word8
charToWord word = (toEnum (fromEnum word)) :: Word8

wordToChar :: Word8 -> Char
wordToChar word = (toEnum (fromEnum word)) :: Char

wordToBooleans :: Word8 -> [Bool]
wordToBooleans word = map (testBit word) $ take 8 $ iterate pred 7

booleansToString :: [Bool] -> String
booleansToString booleans = map wordToChar $ booleansToWord8List booleans

booleansToWord8List :: [Bool] -> [Word8]
booleansToWord8List (a:b:c:d:e:f:g:h:rest) = 
  (
  (boolToWord8 a 7) +
  (boolToWord8 b 6) +
  (boolToWord8 c 5) +
  (boolToWord8 d 4) +
  (boolToWord8 e 3) +
  (boolToWord8 f 2) +
  (boolToWord8 g 1) +
  (boolToWord8 h 0)
  ):(booleansToWord8List rest)
booleansToWord8List _ = []


-- 7 is max position (128/most left/most significant), 0 is least significant
boolToWord8 :: Bool -> Int -> Word8
boolToWord8 True pos = 1*(2^pos)
boolToWord8 False pos = 0


--ghci> testBit (1 :: Word8) 0
--True
--ghci> testBit (1 :: Word8) 1
--False