module Main where


greet = "Hi"
greeted = "There"

main :: IO ()
main = do
  putStrLn "Hello World!"
  putStrLn greet2
    where greet2 = concat [greet, " ", greeted]
