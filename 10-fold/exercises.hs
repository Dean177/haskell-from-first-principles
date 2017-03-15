-- 1. foldr (*) 1 [1..5]

-- b) foldl (flip (*)) 1 [1..5]
-- c) foldl (*) 1 [1..5]

-- 2. 
-- f = (flip (*))
-- foldl f 1 [1..3]
-- foldl f 1 [1 , 2 , 3]
-- (((1 `f` 1) `f` 2) `f` 3)
-- ((1 `f` 2) `f` 3)
-- (2 `f` 3)
-- 6

-- 3.
-- foldr, but not foldl, associates to the right

-- 4.
-- a) reduce structure

-- 5.
a = foldr (++) "" ["woot", "WOOT", "woot"]
b = foldr max "" "fear is the little death"
c = foldr (&&) True [False, True]
d = foldr (||) False [False, True] 
--In its original form nothing from the array will be evaluated

e = foldr ((++) . show) "" [1..5]
f = foldr (const . show) "a" [1..5]
g = foldr const "0" "tacos"
h = foldl (flip const) '0'N "burritos"
i = foldl (flip (const . show)) "z" [1..5]
