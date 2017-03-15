
dividedBy :: Integral a => a -> a -> (a, a)
devidedBy num den = go num den 0
  where go n d count
    | n < d = (count, d)
    | otherwise = go (n - d) d (count + 1)
