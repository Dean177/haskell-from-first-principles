import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf s@(x:xs) (y:ys) =
  ((x == y) && isSubsequenceOf xs ys) || isSubsequenceOf s ys

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (firstLetter:rest) = (toUpper firstLetter : rest)

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map (\ word -> (word, (capitalizeWord word))) (words sentence)

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph sentences = undefined 
