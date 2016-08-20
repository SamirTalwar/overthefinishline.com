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
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import GHC.Generics
import Network.HTTP.Types.URI (encodePathSegments)

import OverTheFinishLine.Dashboard.Enumerations

type Url = Text

data Exception =
    UnauthenticatedUser
  | MissingUser
  | MissingParam Text
  | InvalidAuthenticationCode Text
  | QueryFailure Text

data Response a = AuthenticatedResponse a | UnauthenticatedResponse | Error String
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

  Project
    userId UserId
    name Text
    UniqueProjectName name
    deriving Eq Show

  ProjectRepository
    projectId ProjectId
    name Text
    UniqueProjectRepositoryName name
    deriving Eq Show
|]

instance ToJSON User where toJSON = genericToJSON (stripPrefix "user")

data Me =
  Me {
    meUser :: User,
    meProjects :: [MyProject]
  }
  deriving (Eq, Generic, Show)
instance ToJSON Me where toJSON = genericToJSON (stripPrefix "me")

data MySingleProject =
  MySingleProject {
    mySingleProjectUser :: User,
    mySingleProjectProject :: MyProject
  }
  deriving (Eq, Generic, Show)
instance ToJSON MySingleProject where toJSON = genericToJSON (stripPrefix "mySingleProject")

data MyProject =
  MyProject {
    myProjectName :: Text,
    myProjectUrl :: Url,
    myProjectRepositories :: [Text]
  }
  deriving (Eq, Generic, Show)
instance ToJSON MyProject where toJSON = genericToJSON (stripPrefix "myProject")

data Dashboard =
  Dashboard {
    dashboardNow :: UTCTime,
    dashboardPullRequests :: [PullRequest]
  }
  deriving (Eq, Generic, Show)
instance ToJSON Dashboard where toJSON = genericToJSON (stripPrefix "dashboard")

data PullRequest =
  PullRequest {
    prRepository :: Repository,
    prNumber :: Int,
    prTitle :: Text,
    prUpdatedAt :: UTCTime,
    prUrl :: Url
  }
  deriving (Eq, Generic, Show)
instance ToJSON PullRequest where toJSON = genericToJSON (stripPrefix "pr")

data Repository =
  Repository {
    repoOwner :: Text,
    repoName :: Text,
    repoUrl :: Url
  }
  deriving (Eq, Generic, Show)
instance ToJSON Repository where toJSON = genericToJSON (stripPrefix "repo")

projectUrl :: User -> Project -> Url
projectUrl user project =
  decodeUtf8 $ toStrict $ toLazyByteString
    $ encodePathSegments ["projects", userUsername user, projectName project]

stripPrefix :: String -> Options
stripPrefix prefix = defaultOptions { fieldLabelModifier = stripPrefixFromFields prefix }
  where
  stripPrefixFromFields prefix = lowercaseFirstCharacter . drop (length prefix)
  lowercaseFirstCharacter (first : rest) = Char.toLower first : rest
