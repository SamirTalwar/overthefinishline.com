{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module OverTheFinishLine.Dashboard.Persistence where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import OverTheFinishLine.Dashboard.Model
import OverTheFinishLine.Dashboard.PersistentEnumerations

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  PersistedUser
    name Text
    UniqueName name
    deriving Eq Show

  ServiceCredentials
    userId PersistedUserId
    service ThirdPartyService
    serviceUserId Text
    accessToken ByteString
    UserService userId service
    ServiceUser service serviceUserId
    deriving Eq Show

  Session
    sessionId Text
    expiryTime UTCTime
    userId PersistedUserId
    UniqueSessionId sessionId
    deriving Eq Show
|]
