fibs = 1 : scanl (+) 1 fibs

twentyfibs = take 20 fibs
underhundreFibs = takeWhile (\x -> x < 100) fibs

factorials :: [Integer]
factorials = scanl (*) 1 [1..]
factorial = (!!) factorials
