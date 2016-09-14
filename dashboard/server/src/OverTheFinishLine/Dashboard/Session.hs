{-# LANGUAGE OverloadedStrings #-}

module OverTheFinishLine.Dashboard.Session (
  retrieve,
  store,
  remove
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Maybe as Maybe
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time.Clock as Clock
import Database.Esqueleto
import qualified Web.Spock as Spock

import OverTheFinishLine.Dashboard.Configuration
import OverTheFinishLine.Dashboard.Infrastructure
import OverTheFinishLine.Dashboard.Model

retrieve :: Infrastructure -> ExceptT Failure Context UserId
retrieve infrastructure = do
  now <- liftIO Clock.getCurrentTime
  authId <- Spock.cookie "authId" `orFailure` UnauthenticatedUser
  sessions <- withDatabase infrastructure $ do
    delete $ from $ \session ->
      where_ $ session ^. SessionExpiryTime <. val now
    select $ from $ \session -> do
      where_ $ session ^. SessionAuthId ==. val authId
      return session
  Entity sessionId currentSession <- return (Maybe.listToMaybe sessions) `orFailure` UnauthenticatedUser
  let newExpiryTime = Clock.addUTCTime (sessionTTL infrastructure) now
  withDatabase infrastructure $ update $ \session -> do
    set session [SessionExpiryTime =. val newExpiryTime]
    where_ $ session ^. SessionId ==. val sessionId
  return $ sessionUserId currentSession

store :: UserId -> Infrastructure -> ExceptT Failure Context ()
store userId infrastructure = do
  authId <- decodeUtf8 . Base64.encode <$> liftIO (getRandomBytes 64)
  now <- liftIO Clock.getCurrentTime
  let expiryTime = Clock.addUTCTime (sessionTTL infrastructure) now
  _ <- withDatabase infrastructure $ insert (Session authId expiryTime userId)
  lift $ Spock.setCookie "authId" authId (sessionCookieSettings infrastructure)

remove :: Infrastructure -> ExceptT Failure Context ()
remove infrastructure = do
  authId <- lift $ Spock.cookie "authId"
  when (Maybe.isJust authId) $ do
    withDatabase infrastructure $ delete $ from $ \session ->
      where_ $ session ^. SessionAuthId ==. val (Maybe.fromJust authId)
    lift $ Spock.deleteCookie "authId"

withDatabase :: MonadIO m => Infrastructure -> SqlPersistM a -> m a
withDatabase = flip liftSqlPersistMPool . databaseConnectionPool

orFailure :: Monad m => m (Maybe a) -> e -> ExceptT e m a
maybeValue `orFailure` failure = maybeToExceptT failure (MaybeT maybeValue)

sessionCookieSettings :: Infrastructure -> Spock.CookieSettings
sessionCookieSettings infrastructure =
  Spock.defaultCookieSettings {
    Spock.cs_EOL = Spock.CookieValidFor (sessionTTL infrastructure)
  }

sessionTTL :: Infrastructure -> Clock.NominalDiffTime
sessionTTL = configurationSessionTTL . configuration
