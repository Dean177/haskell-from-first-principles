tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

tensDivmod :: Integral a => a -> a 
tensDivmod x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d2
  where
    xLast = x `div` 10
    d = xLast `div` 10
    d2 = d `mod` 10


foldBool :: a -> a -> Bool -> a
foldBool x y cond = case cond of
  True -> x
  False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y cond
  | cond == True = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g mapper (a, c) = (mapper a, c)
