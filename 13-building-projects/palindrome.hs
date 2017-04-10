import Control.Monad
import Data.Char

alphabetChar character = elem character ['a'..'z']

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let normalised = filter alphabetChar . map toLower $ line1
  if normalised == reverse normalised
  then putStrLn "It's a palindrome"
  else putStrLn "Nope"
