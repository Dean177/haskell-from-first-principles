-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = (aToB a) == b

-- 2.
-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB int a = (aToB a) + (fromInteger int)