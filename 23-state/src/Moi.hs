{-# LANGUAGE InstanceSigs #-}
module Moi where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Show s => Show (Moi s a) where
  show _ = "Moi <function>"

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $
    \s -> let (a, s') = g s
          in (f a , s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi f <*> Moi g = Moi $ \s ->
    let (a, gs) = g s
        (aToB, fs) = f s
    in (aToB a , gs)

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (a, s') = f s
    in runMoi (g a) s'

get' :: Moi s s
get' = Moi $ \x -> (x, x)

put' :: s -> Moi s ()
put' x = Moi $ const ((), x)

exec' :: Moi s a -> s -> s
exec' (Moi sa) = snd . sa

eval' :: Moi s a -> s -> a
eval' (Moi sa) = fst . sa

modify' :: (s -> s) -> Moi s ()
modify' f = Moi $ \s -> ((), f s)