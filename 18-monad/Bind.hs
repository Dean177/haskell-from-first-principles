module Bind where

import Control.Monad (join)
-- fmap :: Monad m => (a -> b) -> m a -> m b
-- join :: Monad m => m (m a) -> m a

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma
