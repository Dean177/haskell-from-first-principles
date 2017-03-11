doggy :: Num a => a -> a -> a
doggy x y = x + y * 10
-- doggy 1 0 
-- 1

-- doggy 1 1
-- 11

-- doggy 2 2 
-- 22

oneIsOne :: Num a => a -> a
oneIsOne = doggy 1
-- oneIsOne 1
-- 11

-- oneIsOne 2
-- 21

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip doggy) 2
-- oneIsTwo 3
-- 23

