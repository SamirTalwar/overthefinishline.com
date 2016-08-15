{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module OverTheFinishLine.Dashboard.Model where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import GHC.Generics

import OverTheFinishLine.Dashboard.Enumerations

type Url = Text

data Exception =
    UnauthenticatedUser
  | MissingUser
  | MissingAuthenticationCode
  | InvalidAuthenticationCode Text
  | QueryFailure Text

data Response a = AuthenticatedResponse a | UnauthenticatedResponse
  deriving (Generic, Show)
instance ToJSON a => ToJSON (Response a) where
  toJSON (AuthenticatedResponse value) =
    let (Object hashMap) = toJSON value
    in Object (HashMap.insert (pack "state") (toJSON (pack "Authenticated")) hashMap)
  toJSON UnauthenticatedResponse = object ["state" .= pack "Unauthenticated"]

unauthenticatedResponse :: Response ()
unauthenticatedResponse = UnauthenticatedResponse

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    username Text
    avatarUrl Url
    UniqueUsername username
    deriving Eq Generic Show

  ServiceCredentials
    userId UserId
    service ThirdPartyService
    serviceUserId Text
    accessToken ByteString
    UserService userId service
    ServiceUser service serviceUserId
    deriving Eq Show

  Session
    sessionId Text
    expiryTime UTCTime
    userId UserId
    UniqueSessionId sessionId
    deriving Eq Show
|]

instance ToJSON User where toJSON = genericToJSON (stripPrefix "user")

data UserProjects =
  UserProjects {
    userProjectsUser :: User,
    userProjectsProjects :: [Project]
  }
  deriving (Generic, Show)
instance ToJSON UserProjects where toJSON = genericToJSON (stripPrefix "userProjects")

data Project =
  Project {
    projectName :: Text,
    projectUrl :: Url
  }
  deriving (Generic, Show)
instance ToJSON Project where toJSON = genericToJSON (stripPrefix "project")

data Dashboard =
  Dashboard {
    dashboardNow :: UTCTime,
    dashboardPullRequests :: [PullRequest]
  }
  deriving (Generic, Show)
instance ToJSON Dashboard where toJSON = genericToJSON (stripPrefix "dashboard")

data PullRequest =
  PullRequest {
    prRepository :: Repository,
    prNumber :: Int,
    prTitle :: Text,
    prUpdatedAt :: UTCTime,
    prUrl :: Url
  }
  deriving (Generic, Show)
instance ToJSON PullRequest where toJSON = genericToJSON (stripPrefix "pr")

data Repository =
  Repository {
    repoOwner :: Text,
    repoName :: Text,
    repoLink :: Url
  }
  deriving (Generic, Show)
instance ToJSON Repository where toJSON = genericToJSON (stripPrefix "repo")

stripPrefix :: String -> Options
stripPrefix prefix = defaultOptions { fieldLabelModifier = stripPrefixFromFields prefix }
  where
  stripPrefixFromFields prefix = lowercaseFirstCharacter . drop (length prefix)
  lowercaseFirstCharacter (first : rest) = Char.toLower first : rest
