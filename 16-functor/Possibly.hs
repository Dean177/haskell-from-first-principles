module Possibly where

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

-- A functor instance that only applies to the 'left' value is not possible as a functor has the kind * -> *
-- The type definition prohibits this, 'Sum' has the kind * -> * -> *