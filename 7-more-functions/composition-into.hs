fiveFrom :: [Integer]
fiveFrom = (take 5 . enumFrom) $ 3

f :: String -> [String] -> String
f = foldr (++)


pipe :: (a -> b) -> (b -> c) -> (a -> c)
pipe = flip (.)


addMulti = (+ 3) `pipe` (* 2) 