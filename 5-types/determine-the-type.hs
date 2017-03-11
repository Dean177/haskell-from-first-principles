module DetermineTheType where

-- 1
qa :: Num a => a
qa = (* 9) 6

qb :: Num a => (Num a, [Char])
qb = head [(0,"doge"),(1,"kitteh")]

qc :: (Integer, [Char])
qc = head [(0 :: Integer ,"doge"),(1,"kitteh")]

qd :: Bool
qd = if False then True else False


qe :: Int
qe = length [1, 2, 3, 4, 5]

qf :: Bool
qf = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- 2
let x2 = 5
let y2 = x2 + 5
let w :: Integer; w = y2 * 10

-- 3
let x3 = 5
let y3 = x3 + 5
let z3 :: Num a => a -> a; z3 y3 = y3 * 10

-- 4
let x4 = 5
let y4 = x4 + 5
let f4 :: Fractional a; f4 = 4 / y4

-- 5
let x = "Julie"
let y = " <3 "
let z = "Haskell"

let f5 :: [Char]; f5 = x ++ y ++ z

