module List where

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
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> xs = flatMap (`fmap` xs) fs

instance Monad List where
  Nil >>= _ = Nil
  (Cons x xs) >>= f = append (f x) (xs >>= f)

