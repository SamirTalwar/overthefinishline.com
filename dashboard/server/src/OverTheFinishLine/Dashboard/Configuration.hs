{-# LANGUAGE OverloadedStrings #-}

module OverTheFinishLine.Dashboard.Configuration where

import qualified Data.ByteString.Char8
import Data.Time.Clock (NominalDiffTime)
import Database.Persist.Postgresql (ConnectionString)
import Network.OAuth.OAuth2 (OAuth2 (OAuth2))
import System.Environment (getEnv, lookupEnv)
import System.FilePath (FilePath)
import System.IO.Error (catchIOError)

data Configuration = Configuration {
  configurationEnvironment :: Environment,
  configurationPort :: Port,
  configurationClientPath :: FilePath,
  configurationGitHubOAuthCredentials :: OAuth2,
  configurationDatabaseConnectionString :: ConnectionString,
  configurationDatabasePoolSize :: Int,
  configurationSessionTTL :: NominalDiffTime,
  configurationSessionStoreInterval :: NominalDiffTime,
  configurationHttpClientTimeoutInSeconds :: Int
}

data Environment = Development | Production
  deriving (Eq, Read, Show)

type Port = Int

readConfiguration =
  Configuration
    <$> readDefaultedEnv "ENVIRONMENT" Production
    <*> readEnv "PORT"
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
      return $ Data.ByteString.Char8.pack $ unwords $ map (\(key, value) -> key ++ "='" ++ value ++ "'") [
        ("host", host),
        ("port", port),
        ("dbname", database),
        ("user", username),
        ("password", password)]
    <*> readEnv "DATABASE_POOL_SIZE"
    <*> (fromInteger <$> readDefaultedEnv "SESSION_TTL" 3600)
    <*> (fromInteger <$> readDefaultedEnv "SESSION_STORE_INTERVAL" 10)
    <*> readDefaultedEnv "HTTP_CLIENT_TIMEOUT" 5
  where
    byteStringEnv name = Data.ByteString.Char8.pack <$> getEnv name

    readEnv :: Read a => String -> IO a
    readEnv name = do
      value <- getEnv name
      readIO value `catchIOError` \theError -> do
        putStrLn $ "Failed to parse the variable " ++ name ++ " with the value \"" ++ value ++ "\"."
        ioError theError

    readDefaultedEnv :: Read a => String -> a -> IO a
    readDefaultedEnv name defaultValue = do
      value <- lookupEnv name
      maybe (return defaultValue) (\value ->
        readIO value `catchIOError` \theError -> do
          putStrLn $ "Failed to parse the variable " ++ name ++ " with the value \"" ++ value ++ "\"."
          ioError theError
        ) value
