stops = "pbtdkg"
vowels = "aeiou"

sts = [(x:(y:(z:[]))) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["duck", "table", "zebra"]
verbs = ["asks", "smells", "wants"]
sentences = [concat [x, " ", y, " ", z] | x <- nouns, y <- verbs, z <- nouns]

averageWordLength :: String -> Double
averageWordLength sentence = (fromIntegral wordLengths) / (fromIntegral wordCount)
  where
    wordLengths = sum (map (length) wordsInSentence)
    wordCount = length wordsInSentence
    wordsInSentence = words sentence

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==) x) False
myElem2 x = any ((==) x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x then x:xs else xs) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\x y -> if (f x y == GT) then x else y) 
