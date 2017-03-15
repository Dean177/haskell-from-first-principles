import Data.Char

lOnly :: String -> String
lOnly = filter $ not . isUpper

capitalize "" = ""
capitalize (a:as) = toUpper a : as

upperilize "" = ""
upperilize (a:as) = toUpper a : upperilize as

firstUpper :: [Char] -> Char
firstUpper = toUpper . head
