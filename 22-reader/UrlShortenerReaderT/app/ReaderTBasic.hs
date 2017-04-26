module ReaderTBasic where

import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, ask, runReaderT)

someFun :: ReaderT Int IO ()
someFun =
  do
    x <- ask
    lift $ print x
    lift $ print x

main :: IO ()
main = runReaderT someFun (5 :: Int)