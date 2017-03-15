summer :: (Eq a, Num a) => a -> a
summer x = go 0 1
  where
    go total index
     | index == x = total
     | otherwise = go (total + index) (index + 1)


multiply :: (Eq a, Num a) => a -> a 
multiply x = go 0 x
  where
    go a b
      | b == 0 = a
      | otherwise = go (a + x) (b - 1)

goF :: Integral a => a -> a -> a -> (a, a)
goF n d count
  | n < d = (count, n)
  | otherwise = goF (n - d) d (count + 1)


dividedByA :: Integral a => a -> a -> (a, a) 
dividedByA num denom = goF num denom 0


dividedByB :: Integral a => a -> a -> (a, a)
dividedByB num den = go num den 0 
  where 
    go n d count 
      | n < d = (count, d)
      | otherwise = go (n - d) d (count + 1)

data DividedResult =
  Result Integer | DividedByZero deriving Show
    
dividedByC :: Integral a => a -> a -> DividedResult
dividedByC num den
  | den == 0 = DividedByZero
  | otherwise = Result $ go num 0
  where
    go n count
      | n < den = count
      | otherwise = go (n - den) (count + 1)

mc91 :: (Num a, Ord a) => a -> a
mc91 x 
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ x + 11
