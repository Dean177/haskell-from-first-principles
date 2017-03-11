import Data.List (sort)

-- 2.
f2 :: Float
-- f2 :: Num a => a
f2 = 1.0

-- 3. both compile
-- f :: Float
f3 :: Fractional a => a
f3 = 1.0

-- 4. both compile
-- f :: Float
f :: RealFrac a => a
f = 1.0

-- 5. both compile
-- freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- 6.
-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x

-- 7.
myX = 1 :: Int

sigmund :: Int -> Int
-- sigmund :: a -> a
sigmund x = myX

-- 8.
sigmund' :: Int -> Int
--sigmund' :: Num a => a -> a
sigmund' x = myX

-- 9. both compile
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10. both compile
-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11.
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

