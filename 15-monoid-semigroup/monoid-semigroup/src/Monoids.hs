module Monoids where

import Control.Monad
import Data.Monoid
import Test.QuickCheck


data Booly a = False' | True' deriving (Eq, Show)

instance Monoid (Booly a) where
  mempty = False'

  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'


data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools) , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool


data Optional a = Some a | None deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = None

  mappend left None = left
  mappend None right = right
  mappend (Some l) (Some r) = Some (l `mappend` r)

  mconcat = foldr mappend mempty


newtype First' a =
  First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' None

  mappend (First' x) (First' None) = First' x
  mappend (First' None) (First' x) = First' x
  mappend (First' x) (First' _) = First' x

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  return (First' (Some a))

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend


newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Show (Mem s a) where
  show (Mem _) = "Mem <function>"

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)
  mappend (Mem f) (Mem g) = Mem $ \x ->
    let (a, x') = f x
        (b, x'') = g x'
    in (a `mappend` b, x'')

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    f <- arbitrary
    return $ Mem f