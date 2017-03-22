{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show
instance TooMany Goats where
  tooMany (Goats n) = n > 43

newtype Snakes =
  Snakes Int deriving (Eq, Show, TooMany)

newtype SpottedCreature = SpottedCreature (Int, String)  deriving Show
instance TooMany SpottedCreature where
    tooMany (SpottedCreature (spotCount, _)) = spotCount > 10

instance TooMany (Int, Int) where
  tooMany = (> 10) . foldr1 (+) 

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany $ a - b 