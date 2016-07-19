{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringC
import Data.ByteString.Lazy (toStrict)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (badRequest400)
import Network.OAuth.OAuth2 as OAuth
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Environment (getEnv)
import System.FilePath
import System.IO.Error
import Web.Spock

import OverTheFinishLine.Dashboard.Model

type Port = Int
data OAuthCredentials = OAuthCredentials {
  clientId :: ByteString,
  clientSecret :: ByteString
}
data Configuration = Configuration {
  port :: Port,
  clientPath :: FilePath,
  gitHubOAuthCredentials :: OAuthCredentials
}
data Session = Session {
  gitHubAccessToken :: OAuth.AccessToken
}
data User = User {
  login :: String
}

instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "login"
  parseJSON _ = mzero

main = readConfiguration >>= server

server (Configuration port clientPath (OAuthCredentials gitHubClientId gitHubClientSecret)) = do
  httpManager <- newManager tlsManagerSettings
  runSpock port $ spock (defaultSpockCfg Nothing PCNoDatabase ()) $ do
    middleware $ staticPolicy (noDots >-> addBase clientPath)
    get "/" $ file "text/html" (clientPath </> "index.html")
    get "/authentication/by/github" $
      redirect $ decodeUtf8 $ authorizationUrl gitHubOAuth
    get "/authorization/by/github" $ do
      code <- param "code"
      case code of
        Nothing ->
          setStatus badRequest400
        Just code -> do
          response <- liftIO $ fetchAccessToken httpManager gitHubOAuth (encodeUtf8 code)
          case response of
            Left failure ->
              text $ decodeUtf8 $ toStrict failure
            Right accessToken -> do
              sessionRegenerateId
              writeSession $ Just (Session accessToken)
              text $ pack $ show accessToken
    get "/dashboard" $ do
      maybeSession <- readSession
      case maybeSession of
        Nothing ->
          json Unauthenticated
        Just session -> do
          response <- liftIO $ authGetJSON httpManager (gitHubAccessToken session) "https://api.github.com/user"
          case response of
            Left failure ->
              text $ decodeUtf8 $ toStrict failure
            Right user -> do
              now <- liftIO getCurrentTime
              json (Dashboard now (login user) [])
  where
    gitHubOAuth = OAuth2 {
      oauthClientId = gitHubClientId,
      oauthClientSecret = gitHubClientSecret,
      oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize?scope=user:email%20repo",
      oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token",
      oauthCallback = Nothing
    }

readConfiguration =
  Configuration
   <$> readEnv "PORT"
   <*> getEnv "CLIENT_PATH"
   <*> (OAuthCredentials
     <$> byteStringEnv "GITHUB_OAUTH_CLIENT_ID"
     <*> byteStringEnv "GITHUB_OAUTH_CLIENT_SECRET")
  where
    byteStringEnv name = ByteStringC.pack <$> getEnv name

    readEnv :: Read a => String -> IO a
    readEnv name = do
      value <- getEnv name
      readIO value `catchIOError` \theError -> do
        putStrLn $ "Failed to parse the variable " ++ name ++ " with the value \"" ++ value ++ "\"."
        ioError theError
