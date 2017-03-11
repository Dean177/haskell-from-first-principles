f :: zed -> Zed -> Blah
--   [0]     [1]    [2]
-- 0: fully pollymorphic
-- 1: Concrete
-- 2: Concrete

f :: Enum b => a -> b -> C
--            [0]  [1]  [2]
-- 0: fully polymorphic
-- 1: constrained polymorphic
-- 2: Concrete


f :: f -> g -> C
--  [0]  [1]  [2]
-- 0: polymorphic
-- 1: polymorphic
-- 2: Concrete