-- import Data.Bool

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

cubeSqr = [(sqr, cube) | sqr <- mySqr, cube <- myCube, sqr < 50, cube < 50]

-- count = foldl (\total _ -> total + 1) 0 cubeSqr
count2 = length cubeSqr


-- boolMap = map (\x -> bool x -x (x == 3))


numnums = length $ filter (\x -> (rem x 3) == 0) [1..30]

filterArticles = filter (\x -> not (elem x ["the", "a", "an"])) . words

myZip :: [a] -> [b] -> [(a,b)]
myZip as bs = case (as, bs) of 
  ([], _) -> []
  (_, []) -> []
  ((a:as), (b:bs)) -> (a, b) : myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs
