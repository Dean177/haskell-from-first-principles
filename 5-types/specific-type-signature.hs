--let f :: a -> a -> a -> a; f = undefined
-- :t f 'a'
-- Char -> Char -> Char

--let g :: a -> b -> c -> b; g = undefined
-- :t g 0 'c' "woot"
-- Char

--let h :: (Num a, Num b) => a -> b -> b; h = undefined
-- :t h 1.0 2
-- Num b
-- I put Integer here initially, the compiler seeks the most general type, and we arent specifying what the type of 2 is

--let jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
-- :t jackal "keyboard" "has the word jackal init"
-- [Char]

--let jackalb :: (Ord a, Eq b) => a -> b -> a; jackalb = undefined
-- :t jackalb "keyboard"
-- (Eq b) => b -> [Char]

--let kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
-- :t kessel 1 2
-- (Ord a, Num a) => a
-- I put Ord a => a initially, forgetting that again, we havent specified a type for the '1' argument, so the compiler
-- Picks the most general type

-- :t kessel 1 (2 :: Integer)
-- (Ord a, Num a) => a

-- :t kessel (1 :: Integer) 2
-- Integer


parametricParamA :: a -> a -> a:t
parametricParamA x y = x
parametricParamA x y = y

parametricParamB :: a -> b -> b
parametricParamB x y = y

myConcat :: [Char] -> [Char]
myConcat x = x ++ "yo"

myMulti :: Fractional a => a -> a
myMulti x = (x / 3) * 5

myTake :: Int -> [Char]
myTake x = take x "hey you"

myCom :: Int -> Bool
myCom x = x > (length [1..10])

myAlph :: Char -> Bool
myAlph x = x < 'z'
