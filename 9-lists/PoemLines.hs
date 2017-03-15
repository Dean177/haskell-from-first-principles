module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful symmetry?" 
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
  ["Tyger Tyger, burning bright" , "In the forests of the night" , "What immortal hand or eye" , "Could frame thy fearful symmetry?"]

notNewline :: Char -> Bool
notNewline = ('\n' /=)

myWords :: String -> [String]
myWords = splitOn ' '

myLines :: String -> [String]
myLines = splitOn '\n'

splitOn :: Char -> String -> [String]
splitOn char sentence = case rest of
  "" -> [sent]
  _ -> [sent] ++ splitOn char (drop 1 rest)
  where
    (sent, rest) = (takeWhile (/= char) sentence, dropWhile (/= char) sentence)

main :: IO ()
main =
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)    