module Instances where

import Data.Monoid

newtype Constant a b = Constant a deriving (Eq, Show)
instance Foldable (Constant a) where
  foldMap _ (Constant x) = mempty

data Two a b = Two a b
instance Foldable (Two a) where
  foldMap f (Two _ y) = f y

data Three a b c = Three a b c
instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldMap f (Three' a y x) = f x <> f y

data Four a b = Four a b b b
instance Foldable (Four a) where
  foldMap f (Four w x y z) = f x <> f y <> f z


filterF :: (Applicative f, Foldable t, Monoid (f a)) =>
  (a -> Bool) -> t a -> f a
filterF predicate = foldMap (\x -> if predicate x then pure x else mempty)

