module Ipv6 where

import Control.Applicative (liftA2)
import Data.Char
import Data.List
import Text.Trifecta
import IPV4

newtype IPV6Address = IPV6Address Integer deriving (Eq, Ord)

-- fromIPV4 :: IPV4Address -> IPV6Address
-- from (IPV4Address x) = IPV6Address x
-- I Guess this is all that is needed here?
-- Unless the book means convert from a string ipv4 to a string of ipv6

powers = (16 ^) . (4 *) <$> [0..7]
toIntGroups :: Integer -> [Integer]
toIntGroups bigNum = reverse $ fst $
  foldr
    (\curr (acc, prevRemainder) ->
      let (unit, remainder) = divMod prevRemainder curr
      in (unit:acc, remainder))
    ([], bigNum)
    powers

intToHexCharacter :: Integer -> String
intToHexCharacter x
  | x < 10 = show x
  | otherwise = case x of
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    15 -> "f"


hexMagnitudes = (16 ^) <$> [0..3]
base10toHex :: Integer -> String
base10toHex x = fst $
  foldr
    (\order (chars, prevRemainder) ->
      let (units, remainder) = divMod prevRemainder order
      in (chars ++ intToHexCharacter units, remainder))
    ("", x)
    hexMagnitudes

instance Show IPV6Address where
  show (IPV6Address x) = intercalate ":" $ base10toHex <$> toIntGroups x

validHexChars :: String
validHexChars = "0123456789abcdefABCDEF"

hexDigitToNumeral :: Char -> Integer
hexDigitToNumeral char = case toLower char of
  '0' -> 0
  '1' -> 1
  '2' -> 2
  '3' -> 3
  '4' -> 4
  '5' -> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9' -> 9
  'a' -> 10
  'b' -> 11
  'c' -> 12
  'd' -> 13
  'e' -> 14
  'f' -> 15


digitToHex :: Char -> (Integer, Integer) -> (Integer, Integer)
digitToHex digit (i, acc) =
  (i + 1, (hexDigitToNumeral digit * (16 ^ i)) + acc)

hexCharsToInteger :: String -> Integer
hexCharsToInteger = snd . foldr digitToHex (0, 0)

intGroupToInteger :: Integer -> (Integer, Integer) -> (Integer, Integer)
intGroupToInteger currentInteger (i, intSum) =
  (i + 1, currentInteger * (65536 ^ i) + intSum)

ipv6ToInteger :: [Integer] -> Integer
ipv6ToInteger = snd . foldr intGroupToInteger (0, 0)

hexChar :: Parser Char
hexChar = oneOf validHexChars

normaliseHexGroups :: [String] -> [String]
normaliseHexGroups hexGroups = case elemIndex "" hexGroups of
  Nothing -> hexGroups
  Just i -> front ++ padding ++ end
    where
      groupLength = length hexGroups
      front = take i hexGroups
      end = drop (i + 1) hexGroups
      paddingSize = 8 - (groupLength - 1)
      padding = replicate paddingSize "0"

parseIpAddress :: Parser IPV6Address
parseIpAddress = do
  hexGroups <- many hexChar `sepBy` char ':'
  let ipv6Groups = hexCharsToInteger <$> normaliseHexGroups hexGroups
  return $ IPV6Address $ ipv6ToInteger ipv6Groups

main = do
  print $ liftA2 (==) (parseString parseIpAddress mempty "0:0:0:0:0:ffff:ac10:fe01") (Success $ IPV6Address 281473568538113)
  print $ liftA2 (==) (parseString parseIpAddress mempty "::ffff:ac10:fe01") (Success $ IPV6Address 281473568538113)
  print $ liftA2 (==) (parseString parseIpAddress mempty "0:0:0:0:0:ffff:cc78:f") (Success $ IPV6Address 281474112159759)
  print $ liftA2 (==) (parseString parseIpAddress mempty "::ffff:cc78:f") (Success $ IPV6Address 281474112159759)
  print $ liftA2 (==) (parseString parseIpAddress mempty "FE80:0000:0000:0000:0202:B3FF:FE1E:8329") (Success $ IPV6Address 338288524927261089654163772891438416681)
  print $ liftA2 (==) (parseString parseIpAddress mempty "FE80::0202:B3FF:FE1E:8329") (Success $ IPV6Address 338288524927261089654163772891438416681)
  print $ liftA2 (==) (parseString parseIpAddress mempty "2001:DB8::8:800:200C:417A") (Success $ IPV6Address 42540766411282592856906245548098208122)
