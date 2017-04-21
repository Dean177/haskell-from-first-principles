module PhhhbbtttEither where

data PhhhbbtttEither a b = Leeft a | Riight b deriving (Eq, Show)

instance Functor (PhhhbbtttEither a) where
  fmap f (Riight x) = Riight (f x)
  fmap _ (Leeft x) = Leeft x

instance Applicative (PhhhbbtttEither a) where
  pure = Riight
  (Leeft x) <*> _ = Leeft x
  _ <*> (Leeft x) = Leeft x
  (Riight f) <*> (Riight x) = Riight (f x)

instance Monad (PhhhbbtttEither a) where
  (Leeft x) >>= _ = Leeft x
  (Riight x) >>= f = f x


