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
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import OverTheFinishLine.Dashboard.Model
import OverTheFinishLine.Dashboard.PersistentEnumerations

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    name Text
    UniqueName name
    deriving Eq Show

  ServiceCredentials
    userId UserId
    service ThirdPartyService
    serviceUserId Text
    accessToken ByteString
    UserService userId service
    ServiceUser service serviceUserId
    deriving Eq Show
|]
