import Data.Char

alphabetLength = length ['A'..'Z']
alphabetStart = ord 'A'

modAlphabet :: Int -> Int
modAlphabet ordChar = alphabetStart + (mod (ordChar - alphabetStart) alphabetLength)

encodeChar :: Char -> Char -> Char
encodeChar keyChar character = chr (modAlphabet (ord character + distance))
  where distance = (ord keyChar) - alphabetStart

dupe :: [a] -> [a]
dupe xs = xs ++ (dupe xs)

encode :: String -> String -> String
encode keyword = zipWith (encodeChar) (dupe keyword)