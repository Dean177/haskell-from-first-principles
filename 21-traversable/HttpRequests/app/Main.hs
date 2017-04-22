module Main where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

mappingGet :: [String] -> [IO (Response ByteString)]
mappingGet = map get

traversedUrls :: [String] -> IO [Response ByteString]
traversedUrls = traverse get

urls :: [String]
urls = ["http://httpbin.com/ip", "http://httpbin.org/bytes/5"]

main :: IO [Response ByteString]
main = traversedUrls urls
