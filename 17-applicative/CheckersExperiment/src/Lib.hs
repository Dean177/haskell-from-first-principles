module Lib where

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x xs)
  | n <= 0 = Nil
  | otherwise = Cons x (take' (n - 1) xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> xs = flatMap (`fmap` xs) fs


newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (repeat' x)
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' fs <*> ZipList' xs = ZipList' (zipWith' ($) fs xs)


data Validation e a = Failured e | Successed a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failured e) = Failured e
  fmap f (Successed x) = Successed (f x)

instance Monoid e => Applicative (Validation e) where
  pure = Successed
  (Failured es) <*> (Failured es') = Failured (es `mappend` es')
  (Failured e) <*> _ = Failured e
  _ <*> (Failured e) = Failured e
  (Successed x) <*> (Successed y) = Successed (x y)
