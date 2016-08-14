{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
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
import Network.HTTP.Types.Status (badRequest400, internalServerError500)
import Network.OAuth.OAuth2
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Environment (getEnv)
import System.FilePath
import System.IO.Error
import Web.Spock

import qualified OverTheFinishLine.Dashboard.GitHub as GitHub
import OverTheFinishLine.Dashboard.Model

type Port = Int
data Configuration = Configuration {
  port :: Port,
  clientPath :: FilePath,
  gitHubOAuthCredentials :: OAuth2
}
data Session = Session {
  user :: GitHub.AuthenticatedUser
}

main = readConfiguration >>= server

server configuration = do
  httpManager <- newManager tlsManagerSettings
  webServer configuration httpManager

webServer (Configuration port clientPath gitHubOAuthCredentials) httpManager =
  runSpock port $ spock (defaultSpockCfg Nothing PCNoDatabase ()) $ do
    middleware $ staticPolicy (noDots >-> addBase clientPath)

    get "/" appHtml

    get "/authentication/by/github" authenticateWithGitHub

    get "/authorization/by/github" $ do
      user <- retrieveGitHubUser
      either handleException store user

    get "/dashboard" $ do
      now <- liftIO getCurrentTime
      user <- readUser
      either handleException (renderDashboard now) user

  where
    appHtml = file "text/html" (clientPath </> "index.html")

    authenticateWithGitHub = redirect $ decodeUtf8 $ authorizationUrl gitHubOAuthCredentials

    store token = do
      sessionRegenerateId
      writeSession $ Just (Session token)
      redirect "/"

    retrieveGitHubUser = runExceptT $ do
      code <- param "code" `orException` MissingAuthenticationCode
      accessToken <- withExceptT (InvalidAuthenticationCode . decodeUtf8 . toStrict) $
        ExceptT $ liftIO $ fetchAccessToken httpManager gitHubOAuthCredentials (encodeUtf8 code)
      user <- withExceptT (QueryFailure . decodeUtf8 . toStrict) $
        ExceptT $ liftIO $ authGetJSON httpManager accessToken "https://api.github.com/user"
      return $ GitHub.AuthenticatedUser accessToken user

    readUser = runExceptT $ do
      session <- readSession `orException` UserIsUnauthenticated
      return $ GitHub.user $ user session

    renderDashboard now user = json (Dashboard now (GitHub.login user) [])

    maybe `orException` exception = maybeToExceptT exception (MaybeT maybe)

    handleException UserIsUnauthenticated =
      json Unauthenticated
    handleException MissingAuthenticationCode =
      setStatus badRequest400
    handleException (InvalidAuthenticationCode message) = do
      setStatus badRequest400
      text message
    handleException (QueryFailure message) = do
      setStatus internalServerError500
      text message

readConfiguration =
  Configuration
   <$> readEnv "PORT"
   <*> getEnv "CLIENT_PATH"
   <*> (OAuth2
     <$> byteStringEnv "GITHUB_OAUTH_CLIENT_ID"
     <*> byteStringEnv "GITHUB_OAUTH_CLIENT_SECRET"
     <*> pure "https://github.com/login/oauth/authorize?scope=user:email%20repo"
     <*> pure "https://github.com/login/oauth/access_token"
     <*> pure Nothing)
  where
    byteStringEnv name = ByteStringC.pack <$> getEnv name

    readEnv :: Read a => String -> IO a
    readEnv name = do
      value <- getEnv name
      readIO value `catchIOError` \theError -> do
        putStrLn $ "Failed to parse the variable " ++ name ++ " with the value \"" ++ value ++ "\"."
        ioError theError
