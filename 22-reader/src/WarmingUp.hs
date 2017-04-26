module WarmingUp where

import Control.Applicative
import Data.Char

boop :: Integer -> Integer
boop = (*2)
doop:: Integer -> Integer
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = fmap rev cap

tupledA :: String -> (String, String)
tupledA = liftA2 (,) rev cap

tupledMDo :: String -> (String, String)
tupledMDo = do
  reversed <- rev
  capitalized <- cap
  return (reversed, capitalized)

tupledMBind :: String -> (String, String)
tupledMBind =
  rev >>= \reversed ->
    cap >>= \capitalised ->
      return (reversed, capitalised)
