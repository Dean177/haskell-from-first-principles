{-# LANGUAGE InstanceSigs #-}
module Ask where

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 fun x y = fun <$> x <*> y

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= arb = Reader $ \r -> (runReader $ arb (ra r)) r

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName
, dogName :: DogName
, address :: Address
} deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName
, dogsAddress :: Address
} deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogA :: Person -> Dog
getDogA = Dog <$> dogName <*> address

getDogM :: Person -> Dog
getDogM = do
  name <- dogName
  addy <- address
  return $ Dog name addy


getDogR :: Reader Person Dog
getDogR = Dog <$> Reader dogName <*> Reader address
