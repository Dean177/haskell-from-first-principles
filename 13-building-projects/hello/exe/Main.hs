module Main where

import DogsRule
import Hello
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Whats your name:"
  name <- getLine
  sayHello name
  dogs

