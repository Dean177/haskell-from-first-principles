functionH :: [x] -> x
functionH (x:_) = x

functionC :: (Ord x) => x -> x -> Bool
functionC x y = if (x > y) then True else False

functionS :: (x, y) -> y
functionS (x, y) = y