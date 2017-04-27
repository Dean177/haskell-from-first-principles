import Control.Applicative (liftA2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Moi

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Moi Char (Int, Int, Int))
  quickBatch $ applicative (undefined :: Moi Char (Int, Int, Int))
  -- TODO QuickCheck fails the applicative instance:
  -- interchange:  *** Failed! Falsifiable (after 1 test and 1 shrink):
  -- Moi <function>
  -- 0
  -- 'a'
  -- RHS
  quickBatch $ monad (undefined :: Moi Char (Int, Int, Int))

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Moi s a) where
  arbitrary = do
    f <- arbitrary
    return $ Moi f

instance (Show s, Arbitrary s, EqProp a, EqProp s) => EqProp (Moi s a) where
  (Moi f) =-= (Moi f') = property (liftA2 (=-=) f f')