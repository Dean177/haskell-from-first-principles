{-# LANGUAGE ViewPatterns #-}
module FunctorLaws where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorComposition :: (Functor f,  Eq (f c)) =>
  (a -> b) -> (b -> c) -> f a -> Bool
functorComposition f g x = fmap (g . f) x == fmap g (fmap f x)

functorCompose' :: (Eq (f c), Functor f) =>
 f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  fmap (g . f) x == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x


data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return $ Pair x x

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y


data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z


data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z


data Four a b = Four a a a b deriving (Eq, Show)

instance Functor (Four a) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

-- No functor can be implememnted for `data Trivial = Trivial` as there is no structure, its just a value

main :: IO ()
main = do
  quickCheck (functorCompose' :: Identity Int -> IntToInt -> IntToInt -> Bool)

  quickCheck (functorCompose' :: Pair Int -> IntToInt -> IntToInt -> Bool)

  quickCheck (functorCompose' :: Two String Int -> IntToInt -> IntToInt -> Bool)

  quickCheck (functorCompose' :: Three String String Int -> IntToInt -> IntToInt -> Bool)

  quickCheck (functorCompose' :: Three' String Int -> IntToInt -> IntToInt -> Bool)

  quickCheck (functorCompose' :: Four String Int -> IntToInt -> IntToInt -> Bool)

