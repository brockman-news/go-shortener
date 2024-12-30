{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.IO.Class (liftIO)
import Data.Digest.Pure.SHA (sha256, integerDigest)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.ByteString.Lazy.Char8 as BL
import Database.Redis (Connection, checkedConnect, runRedis, set, get, defaultConnectInfo, ConnectInfo(connectPort), PortID(UnixSocket))
import Network.HTTP.Types (status200, status302, status400, status404, hLocation, hContentType)
import Network.URI
import Network.Wai (Application, Request, responseLBS, pathInfo, queryString, strictRequestBody, requestMethod)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse
import Numeric (showIntAtBase)
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE

data Settings = Settings
  { keyLength :: Int
  , port :: Int
  , endpoint :: URI
  , redisSocket :: Maybe FilePath
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
    run (port settings) (app conn settings)

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
                    let shortenerEndpoint = endpoint settings
                    let shortUrl = packUri shortenerEndpoint {uriPath = '/':shortKey}
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
