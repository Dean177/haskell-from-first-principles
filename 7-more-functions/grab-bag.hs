-- 1.
mt :: Num a => a -> a -> a -> a

mth0 :: mth
mTh0 x y z = x * y * z

mth1 :: mth
mTh1 x y = \z -> x * y * z

mth2 :: mth
mTh2 x = \y -> \z -> x * y * z

mth3 :: mth
mTh3 = \x -> \y -> \z -> x * y * z

-- 2. d) Num a => a -> a -> a

-- 3. a)
addOneIfOdd = \n -> case odd n of
  True -> f n
  False -> n
  where f n = n + 1

-- 3. b)
addFive = \x -> \y -> ((if x > y then y else x) + 5)

--3. c)
mflip f x y = f y x
