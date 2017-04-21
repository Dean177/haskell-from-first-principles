import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Sum
import Nope
import PhhhbbtttEither
import List


main :: IO ()
main = do
--   quickBatch $ applicative (undefined :: Sum Char (Int, Int, Int))
--   quickBatch $ functor (undefined :: Sum Char (Int, Int, Int))
--   quickBatch $ monad (undefined :: Sum Char (Int, Int, Int))

--   quickBatch $ applicative (undefined :: Nope (Int, Int, Int))
--   quickBatch $ functor (undefined :: Nope (Int, Int, Int))
--   quickBatch $ monad (undefined :: Nope (Int, Int, Int))

--   quickBatch $ applicative (undefined :: PhhhbbtttEither Char (Int, Int, Int))
--   quickBatch $ functor (undefined :: PhhhbbtttEither Char (Int, Int, Int))
--   quickBatch $ monad (undefined :: PhhhbbtttEither Char (Int, Int, Int))

  quickBatch $ applicative (undefined :: List (Int, Int, Int))
  quickBatch $ functor (undefined :: List (Int, Int, Int))
  quickBatch $ monad (undefined :: List (Int, Int, Int))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    first <- arbitrary
    second <- arbitrary
    elements [First first, Second second]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq


instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq


instance (Arbitrary a, Arbitrary b) =>  Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    left <- arbitrary
    right <- arbitrary
    elements [Leeft left, Riight right]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where
  (=-=) = eq


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = take' 300 xs
      ys' = take' 300 ys
