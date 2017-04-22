{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative (liftA2, liftA3)
import Control.Monad (liftM, liftM3)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant x) = mempty

instance Traversable (Constant a) where
  traverse f (Constant x) = pure $ Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    x <- arbitrary
    return $ Constant x

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq


data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance (Arbitrary a) =>  Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return Nada), (10, return $ Yep x)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq


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

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> xs = flatMap (`fmap` xs) fs

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = liftA2 append fListB fListRest
    where
      fListB = fmap pure (f x) -- :: f (List B)
      fListRest = traverse f xs -- :: f (List B)

arbList :: Arbitrary a => Int -> Gen (List a)
arbList 0 = return Nil
arbList n | n > 0 = do
  x <- arbitrary
  (Cons x) <$> (arbList $ n - 1)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized arbList



instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = take' 300 xs
      ys' = take' 300 ys


data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>  Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y <> f z

instance Traversable (Three' a) where
  traverse f (Three' x y z) = liftA2 (Three' x) (f y) (f z)

instance (Arbitrary a, Arbitrary b) =>  Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S nx x) = S (fmap f nx) (f x)

instance Foldable n => Foldable (S n) where
  foldMap f (S nx x) = foldMap f nx <> f x

instance Traversable n => Traversable (S n) where
  traverse f (S nx x) = liftA2 S (traverse f nx) (f x)

instance (Arbitrary a) =>  Arbitrary (S Identity a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ S (Identity x) y

instance Eq a => EqProp (S Identity a) where (=-=) = eq


data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = return Empty
arbTree n | n > 0 = oneof [liftM Leaf arbitrary, liftM3 Node subtree arbitrary subtree]
  where subtree = arbTree (n `div` 2)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = (foldMap f l) <> (f x) <> (foldMap f r)

  foldr _ acc Empty = acc
  foldr f acc (Leaf x) = f x acc
  foldr f acc (Node left x right) = foldr f (f x rightFolded) left
    where rightFolded = foldr f acc right

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) = liftA3 Node (traverse f l) (f x) (traverse f r)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Constant Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: List (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Three Int Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Three' Int (Int, String, [Int]))
  quickBatch $ traversable (undefined :: S Identity (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))