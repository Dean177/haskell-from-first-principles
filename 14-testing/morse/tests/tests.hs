module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

charGen :: Gen Char
charGen = elements $ M.keys letterToMorse

morseGen :: Gen Morse
morseGen = elements $ M.elems letterToMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> (charToMorse c >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_thereAndBackAgain