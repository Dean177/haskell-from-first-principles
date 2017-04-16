module Lookups where

import Data.List (elemIndex)

-- 1.
added :: Maybe Integer
added = (+ 3) <$> lookup (3::Integer) (zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup (3::Integer) $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup (2::Integer) $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x :: Maybe Int
x = elemIndex (3::Integer) [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex (4::Integer) [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

-- 4.
xs :: [Integer]
xs = [1,2,3]

ys :: [Integer]
ys = [4,5,6]

x' :: Maybe Integer
x' = lookup (3::Integer) $ zip xs ys

y'' :: Maybe Integer
y'' = lookup (2::Integer) $ zip xs ys

summed :: Maybe Integer
summed = pure sum <*> ((,) <$> x' <*> y'')