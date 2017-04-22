module LibraryFunctions where

import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product =  getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (== x))

-- Data.Monoid.Ord had something similar but with a Bounded constraint.
-- Was I supposed to do this or is there a simpler way?
newtype Min a = Min { getMin :: Maybe a } deriving (Eq, Show)
instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  Min Nothing `mappend` x = x
  x `mappend` Min Nothing = x
  mappend (Min x) (Min y) = Min $ min x y

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = getMin . foldMap (Min . Just)

newtype Max a = Max { getMax :: Maybe a } deriving (Eq, Show)
instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  Max Nothing `mappend` x = x
  x `mappend` Max Nothing = x
  (Max x) `mappend` (Max y) = Max $ max x y

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = getMax . foldMap (Max . Just)

null' :: (Foldable t) => t a -> Bool
null' = not . getAny . foldMap (Any . const True)

length :: (Foldable t) => t a -> Int
length = getSum . foldMap (const $ Sum 1)

toList :: (Foldable t) => t a -> [a]
toList = foldMap (:[])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap (<> mempty)

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\ x y -> f x <> y) mempty
