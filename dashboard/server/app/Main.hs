{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace (traceShowId)
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
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
import qualified Database.Persist as Database
import Database.Persist ((=.))
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool)
import Database.Persist.Sql hiding (get)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (badRequest400, internalServerError500)
import Network.OAuth.OAuth2 as OAuth2
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Environment (getEnv)
import System.FilePath
import System.IO.Error
import Web.Spock

import qualified OverTheFinishLine.Dashboard.GitHub as GitHub
import OverTheFinishLine.Dashboard.Model
import OverTheFinishLine.Dashboard.Persistence

type Port = Int
data Configuration = Configuration {
  configurationPort :: Port,
  configurationClientPath :: FilePath,
  configurationGitHubOAuthCredentials :: OAuth2,
  configurationDatabaseConnectionString :: ConnectionString,
  configurationDatabasePoolSize :: Int
}
data Session = Session {
  sessionUserId :: UserId
}

main :: IO ()
main = readConfiguration >>= server

server :: Configuration -> IO ()
server configuration =
  runStderrLoggingT $ withPostgresqlPool databaseConnectionString databasePoolSize $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ runMigration migrateAll
    httpManager <- newManager tlsManagerSettings
    webServer configuration pool httpManager
  where
    databaseConnectionString = configurationDatabaseConnectionString configuration
    databasePoolSize = configurationDatabasePoolSize configuration

webServer (Configuration port clientPath gitHubOAuthCredentials _ _) databaseConnectionPool httpManager =
  runSpock port $ spock (defaultSpockCfg Nothing PCNoDatabase ()) $ do
    middleware $ staticPolicy (noDots >-> addBase clientPath)

    get "/" appHtml

    get "/authentication/by/github" authenticateWithGitHub

    get "/authorization/by/github" $ do
      userId <- storeUser
      either handleException storeSession userId

    get "/dashboard" $ do
      now <- liftIO getCurrentTime
      user <- readUser
      either handleException (renderDashboard now) user

  where
    appHtml = file "text/html" (clientPath </> "index.html")

    authenticateWithGitHub = redirect $ decodeUtf8 $ authorizationUrl gitHubOAuthCredentials

    storeSession userId = do
      sessionRegenerateId
      writeSession $ Just (Session userId)
      redirect "/"

    storeUser = runExceptT $ do
      code <- param "code" `orException` MissingAuthenticationCode
      accessToken <- withExceptT (InvalidAuthenticationCode . decodeUtf8 . toStrict) $
        ExceptT $ liftIO $ fetchAccessToken httpManager gitHubOAuthCredentials (encodeUtf8 code)
      (GitHub.User gitHubUserId gitHubLogin) <- withExceptT (QueryFailure . decodeUtf8 . toStrict) $
        ExceptT $ liftIO $ authGetJSON httpManager accessToken "https://api.github.com/user"
      let accessTokenString = OAuth2.accessToken accessToken
      withDatabase $ do
        let serviceUser = ServiceUser GitHub gitHubUserId
        serviceCredentials <- Database.getBy serviceUser
        case serviceCredentials of
          Nothing -> do
            userId <- Database.insert $ User gitHubLogin
            Database.insert $ ServiceCredentials userId GitHub gitHubUserId accessTokenString
            return userId
          Just (Database.Entity serviceCredentialsId (ServiceCredentials userId _ _ _)) -> do
            Database.update serviceCredentialsId [ServiceCredentialsAccessToken =. accessTokenString]
            return userId

    readUser = runExceptT $ do
      userId <- sessionUserId <$> readSession `orException` UnauthenticatedUser
      withDatabase (Database.get userId) `orException` MissingUser

    renderDashboard now (User username) = json (Dashboard now username [])

    maybe `orException` exception = maybeToExceptT exception (MaybeT maybe)

    handleException UnauthenticatedUser =
      json Unauthenticated
    handleException MissingUser =
      json Unauthenticated
    handleException MissingAuthenticationCode =
      setStatus badRequest400
    handleException (InvalidAuthenticationCode message) = do
      setStatus badRequest400
      text message
    handleException (QueryFailure message) = do
      setStatus internalServerError500
      text message

    withDatabase :: MonadIO m => SqlPersistM a -> m a
    withDatabase = flip liftSqlPersistMPool databaseConnectionPool

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
    <*> do
      host <- getEnv "DATABASE_HOST"
      port <- getEnv "DATABASE_PORT"
      database <- getEnv "DATABASE_DASHBOARD_NAME"
      username <- getEnv "DATABASE_DASHBOARD_USERNAME"
      password <- getEnv "DATABASE_DASHBOARD_PASSWORD"
      return $ traceShowId $ ByteStringC.pack $ unwords $ map (\(key, value) -> key ++ "='" ++ value ++ "'") [
        ("host", host),
        ("port", port),
        ("dbname", database),
        ("user", username),
        ("password", password)]
    <*> readEnv "DATABASE_POOL_SIZE"
  where
    byteStringEnv name = ByteStringC.pack <$> getEnv name

    readEnv :: Read a => String -> IO a
    readEnv name = do
      value <- getEnv name
      readIO value `catchIOError` \theError -> do
        putStrLn $ "Failed to parse the variable " ++ name ++ " with the value \"" ++ value ++ "\"."
        ioError theError
