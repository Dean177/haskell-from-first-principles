myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x) -> (a, z)

myFunc xToY yToZ _ (a, x) = (a, z)
  where z = yToZ (xToY x)

i :: a -> a
i a = a

c :: a -> b -> a
c a _ = a

c'' :: b -> a -> b
c'' = c

c' :: a -> b -> b
c' _ b = b

r :: [a] -> [a]
r a = reverse a

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)

a :: (a -> c) -> a -> a
a _ a = a


a' :: (a -> b) -> a -> b
a' aToB a = aToB a