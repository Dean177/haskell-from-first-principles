module StopVowelStop where

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

sts :: [(Char, Char, Char)]
sts = liftA3 (,,) stops vowels stops
