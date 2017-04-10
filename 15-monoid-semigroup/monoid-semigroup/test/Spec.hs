import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck
import Monoids
import SemiGroups

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a `mappend` (b `mappend` c) == (a `mappend` b) `mappend` c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type TwoInts = Two Integer Integer
type TwoAssoc = TwoInts -> TwoInts -> TwoInts -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type FunctionAssociativity x c = x -> x -> x -> c -> Bool

semigroupCombineAssociativity :: (Eq b, Semigroup b) => FunctionAssociativity (Combine a b) a
semigroupCombineAssociativity f g h c = unCombine ((f <> g) <> h) c == unCombine (f <> (g <> h)) c

semigroupCompAssociativity f g h x =
 unComp (f <> (g <> h)) x == unComp ((f <> g) <> h) x

monoidMemAssociativity f g h x =
  runMem (f `mappend` (g `mappend` h)) x == runMem ((f `mappend` g) `mappend` h) x

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  quickCheck (monoidAssoc :: String -> String -> String -> Bool)
  quickCheck (monoidLeftIdentity :: String -> Bool)
  quickCheck (monoidRightIdentity :: String -> Bool)

--   quickCheck (monoidAssoc :: BullMappend)
--   quickCheck (monoidLeftIdentity :: Bull -> Bool)
--   quickCheck (monoidRightIdentity :: Bull -> Bool)

  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

  quickCheck (semigroupAssoc :: TrivialAssoc)

  quickCheck (semigroupAssoc :: BoolConjAssoc)

  quickCheck (semigroupCombineAssociativity :: FunctionAssociativity (Combine Int String) Int)

  quickCheck (semigroupCompAssociativity :: FunctionAssociativity (Comp Int) Int)

  print $ runMem (f' `mappend` mempty) 0
  print $ runMem (mempty `mappend` f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' `mappend` mempty) 0 == runMem f' 0
  print $ runMem (mempty `mappend` f') 0 == runMem f' 0

  quickCheck (monoidMemAssociativity :: FunctionAssociativity (Mem String String) String)