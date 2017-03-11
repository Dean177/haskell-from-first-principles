data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1. Nope
-- phew = Papu "chases" True
phew = Papu (Rocks "chases") (Yeah True)

-- 2. Yes
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. Yes
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4. No, lacks Ord
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'