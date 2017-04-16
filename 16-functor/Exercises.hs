{-# LANGUAGE FlexibleInstances #-}
module Exercises where

import GHC.Arr

-- 1. Bool cannot have a Functor written for it as there it has the kind *
-- 2.
data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)
instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x) = True' (f x)

-- 3. the instance would be exactly the same as the one above

--4.
newtype Mu f = InF { outF :: f (Mu f) }
-- :k Mu = (* -> *) -> *
-- I did actually try to write the instance but
-- `instance Functor Mu where`
-- will give
-- Expected kind ‘* -> *’, but ‘Mu’ has kind ‘(* -> *) -> *’

--5.
data D = D (Array Word Word) Int Int
-- :k D = * so no functor here

-- 1.
data Sum b a = First a | Second b
instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

 -- 2.
data Company a b c = DeepBlue a b | Something c
instance Functor (Company a b) where
  fmap f (Something z) = Something (f z)
  fmap _ (DeepBlue x y) = DeepBlue x y

-- 3.
data More a b = L b a b | R a b a deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L b a b') = L (f b) a (f b')
  fmap f (R a b a') = R a (f b) a'


-- 1.
data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Bloor y) = Bloor (f y)

-- 2.
newtype K a b = K a deriving (Eq, Show)
instance Functor (K a) where
  fmap _ (K x) = K x

-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip K a) where
  fmap f (Flip (K y)) = Flip (K (f y))

-- 4.
newtype EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

-- 5.
newtype LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- 7.
data IgnoreOne f g a b = IgnoreSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething x y) = IgnoreSomething x (fmap f y)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

-- 9.
data List a = Nil | Cons a (List a)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)¡

-- 10.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11.
data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str (f x)
  fmap f (Read g) = Read (f . g)
