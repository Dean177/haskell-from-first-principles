{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty
import Lib


alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen = replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortUri uri =
  R.runRedis conn $ R.set shortUri uri

getURI  :: R.Connection
        -> BC.ByteString
        -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortUri = R.runRedis conn $ R.get shortUri

linkShorty :: String -> String
linkShorty shorty =
  concat ["<a href=\"", shorty, "\">Copy and paste your short URL</a>"]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [TL.pack (show resp), " shorty is: ", TL.pack (linkShorty shawty) ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri = TL.concat [ uri, " wasn't a url, did you forget 'http://'?"]

shortyDupeShortening :: TL.Text -> TL.Text -> TL.Text
shortyDupeShortening uri code = TL.concat [uri, " was shortened to: ", code, "Which allready exists!"]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs = TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

byteStringToText :: BC.ByteString -> TL.Text
byteStringToText bs = TL.fromStrict (decodeUtf8 bs)

app :: R.Connection -> ScottyM ()
app redisConnection = do
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Nothing -> text (shortyAintUri uri)
      Just _ -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' = encodeUtf8 (TL.toStrict uri)

        preExistingUri <- liftIO (getURI redisConnection shorty)
        case preExistingUri of
          Left reply -> text (TL.pack (show reply))
          Right mbBS -> case mbBS of
            Just bs -> html (shortyDupeShortening uri shawty)
            Nothing -> do
             resp <- liftIO (saveURI redisConnection shorty uri')
             html (shortyCreated resp shawty)

  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI redisConnection short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound $ byteStringToText bs)

redisConnection :: R.ConnectInfo
redisConnection = R.defaultConnectInfo {
  R.connectAuth = Nothing,
  R.connectHost = "192.168.99.100",
  R.connectPort = R.PortNumber 32768,
  R.connectDatabase = 0,
  R.connectMaxConnections = 50,
  R.connectMaxIdleTime = 30
}

main :: IO ()
main = do
  redisConnection <- R.connect redisConnection
  scotty 3000 (app redisConnection)
