module StringManipulation where

dropFirst str = drop 1 str
dropFirstCur = drop 1

withExclaim :: String -> String
withExclaim str = str ++ "!"

takeThird :: String -> Char
takeThird x = x !! 2

letterAtIndex :: Int -> Char
letterAtIndex index = "Curry is awesome" !! index

revrs :: String -> String
revrs input = concat [awesome, " ", is, " ", curry]
  where
    curry = take 5 input
    is = take 2 (drop 6 input)
    awesome = take 7 (drop 9 input)


main :: IO ()
main = print $ revrs "Curry is awesome"
