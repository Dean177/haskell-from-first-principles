import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Lib

data Bull = Fools | Twoo deriving (Eq, Show)
instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools) , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where
  (=-=) = eq

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = take' 300 xs
      ys' = take' 300 ys

instance Eq a => EqProp (ZipList' a) where
  (ZipList' xs) =-= (ZipList' ys) = xs =-= ys

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    x <- arbitrary
    return $ ZipList' x

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq


instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    errs <- arbitrary
    x <- arbitrary
    frequency [(1, return (Failured errs)), (1, return (Successed x))]


main :: IO ()
main =
--   quickBatch (monoid Twoo)
--   quickBatch (applicative (Cons ("a", "b", 1::Int) Nil))
--   quickBatch (applicative (ZipList' (Cons ("a", "b", 1::Int) Nil)))
  quickBatch (applicative (undefined :: Validation [String] (Int, String, Int)))