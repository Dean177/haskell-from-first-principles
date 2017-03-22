data Quantum =
  Yes | No | Both deriving (Eq, Show)


convert :: Quantum -> Bool
convert Yes = True
Convert No = True
Convert Both = True

convert1 Yes = False
convert1 No = False
convert1 Both = False

convert2 Yes = True
convert2 No = False
convert2 Both = False

convert3 Yes = False
convert3 No = True
convert3 Both = False

convert4 Yes = False
convert4 No = False
convert4 Both = True

convert5 Yes = True
convert5 No = True
convert5 Both = False

convert6 Yes = False
convert6 No = True
convert6 Both = True

convert7 Yes = True
convert7 No = False
convert7 Both = True

-- 1. eQuad :: Either Quad Quad = 8
-- 2. prodQuad :: (Quad, Quad) = 16
-- 3. funcQuad :: Quad -> Quad = 256
-- 4. prodTBool :: (Bool, Bool, Bool) = 8
-- 5. gTwo :: Bool -> Bool -> Bool = 16
-- 6. fTwo :: Bool -> Quad -> Quad = 65536