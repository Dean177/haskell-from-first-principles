module Ipv4 where

import Data.Word
import Data.Int
import Text.Trifecta

newtype IPV4Address = IPV4Address Word32 deriving (Eq, Ord, Show)

ipv4ToBase10 :: (Int, Int, Int, Int) -> Word32
ipv4ToBase10 (a, b, c, d) =
  (fromIntegral a * (256 ^ 3)) +
  (fromIntegral b * (256 ^ 2)) +
  (fromIntegral c * 256) +
  fromIntegral d

parseNumberGroup :: Parser Int
parseNumberGroup = do
  num <- natural
  if num > 255 || num < 0
  then fail "out of bounds"
  else return $ fromInteger num

parseIpAddress :: Parser IPV4Address
parseIpAddress = do
  a <- parseNumberGroup
  char '.'
  b <- parseNumberGroup
  char '.'
  c <- parseNumberGroup
  char '.'
  d <- parseNumberGroup
  return $ IPV4Address $ ipv4ToBase10 (a, b, c, d)
