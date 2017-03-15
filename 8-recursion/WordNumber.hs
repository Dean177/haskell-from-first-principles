module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "fail"


didgits :: Int -> [Int]
didgits num = go num []
  where 
    go n digis = case (div n 10) of
      0 -> [n] ++ digis
      _ -> go (div n 10) ((mod n 10 :[]) ++ digis) 

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" $ map digitToWord (didgits n)
