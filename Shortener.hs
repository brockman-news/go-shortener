{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Digest.Pure.SHA (integerDigest, sha256)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text.Encoding as TE
import Database.Redis (ConnectInfo (connectPort), Connection, PortID (UnixSocket), checkedConnect, defaultConnectInfo, get, runRedis, set)
import Network.HTTP.Types (hContentType, hLocation, status200, status302, status400, status404)
import Network.URI
import Network.Wai (Application, Request, pathInfo, queryString, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Parse
import Numeric (showIntAtBase)
import System.Environment (lookupEnv)

data Settings = Settings
  { keyLength :: Int,
    port :: Int,
    endpoint :: URI,
    redisSocket :: Maybe FilePath
  }

getSettings :: IO Settings
getSettings = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  keyLength <- maybe 8 read <$> lookupEnv "KEY_LENGTH"
  maybeEndpointString <- lookupEnv "ENDPOINT"
  redisSocket <- lookupEnv "REDIS_SOCKET"
  let defaultEndpoint = fromJust $ parseAbsoluteURI ("http://localhost:" ++ show port ++ "/")
  let endpoint = fromMaybe defaultEndpoint $ parseAbsoluteURI =<< maybeEndpointString
  return Settings {..}

main :: IO ()
main = do
  settings <- getSettings
  conn <- checkedConnect $ maybe defaultConnectInfo (\socketPath -> defaultConnectInfo {connectPort = UnixSocket socketPath}) $ redisSocket settings
  putStrLn $ "Starting server on http://localhost:" ++ show (port settings)
  run (port settings) (logStdout $ app conn settings)

app :: Connection -> Settings -> Application
app conn settings req respond = do
  case (requestMethod req, pathInfo req) of
    ("GET", [short]) -> do
      let key = TE.encodeUtf8 short
      res <- runRedis conn $ get key
      case res of
        Right (Just url) ->
          respond $ responseLBS status302 [(hLocation, url)] ""
        _ ->
          respond $ responseLBS status404 [(hContentType, "text/plain")] "URL not found"
    ("POST", []) -> do
      (params, _) <- parseRequestBody lbsBackEnd req
      let uriCandidate = validateUri =<< lookup "uri" params
      case uriCandidate of
        Nothing ->
          respond $ responseLBS status400 [(hContentType, "text/plain")] "Invalid URI"
        Just validUri -> do
          let shortKey = take (keyLength settings) $ sha256Hash validUri
          _ <- runRedis conn $ set (B.pack shortKey) (BL.toStrict validUri)
          putStrLn $ show shortKey <> " -> " <> show validUri
          let shortenerEndpoint = endpoint settings
          let shortUrl = packUri shortenerEndpoint {uriPath = '/' : shortKey}
          respond $ responseLBS status200 [(hContentType, "text/plain")] shortUrl
    _ -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request"

packUri :: URI -> BL.ByteString
packUri u = BL.pack (uriToString id u "")

validateUri :: B.ByteString -> Maybe BL.ByteString
validateUri = fmap packUri . parseURI . B.unpack

base32 :: Integer -> String
base32 i = showIntAtBase 32 (alphabet !!) i ""
  where
    alphabet = "0123456789abcdefghilmnopqrstvxyz"

sha256Hash :: BL.ByteString -> String
sha256Hash = base32 . integerDigest . sha256
