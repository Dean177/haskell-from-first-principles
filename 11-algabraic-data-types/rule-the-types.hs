data Doggies a = Husky a | Mastiff a deriving (Eq, Show)
-- 1. Doggies is a type constructor
-- 2. :k Doggies == * -> *
-- 3. :k Doggies String == *
-- 4. :t Husky 10 == Num a => Doggies a
-- 5. :t Husky (10 :: Integer) == Doggies Integer
-- 6. :t Mastiff "Scooby doo" == Doggies [Char]

data DogueDeBordeaux doge = DogueDeBordeaux doge
-- 7. DogueDeBordeaux is both a type constructor and a data constructor (depends on which side of the '=' we are talking about. Left = Type, Right = Data)
-- 8. :t DogueDeBorduex == doge -> DogueDeBordeaux doge
-- 9. :t DogueDeBordeaux "doggie!" == DogeDeBordeux [Char]
