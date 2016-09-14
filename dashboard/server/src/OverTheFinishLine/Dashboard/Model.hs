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
import Database.Persist.TH
import GHC.Generics
import Network.HTTP.Types.URI (encodePathSegments)

import OverTheFinishLine.Dashboard.Enumerations

type Url = Text

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
    authId Text
    expiryTime UTCTime
    userId UserId
    UniqueAuthId authId
    deriving Eq Show

  Project
    userId UserId
    name Text
    UniqueProjectNameByUser userId name
    deriving Eq Show

  ProjectRepository
    projectId ProjectId
    name Text
    UniqueProjectRepositoryNameByProject projectId name
    deriving Eq Show
|]

data Failure =
    UnauthenticatedUser
  | MissingUser
  | MissingProject { failureProject :: Text }
  | MissingParam { failureParam :: Text }
  | QueryFailure { failureMessage :: Text }
  | RequestFailure { failureUrl :: Url, failureMessage :: Text }
  deriving (Eq, Generic, Show)
instance ToJSON Failure where toJSON = genericToJSON (stripPrefix "failure")

data Response a = AuthenticatedResponse [Failure] a | UnauthenticatedResponse
  deriving (Eq, Show)
instance ToJSON a => ToJSON (Response a) where
  toJSON (AuthenticatedResponse [] value) =
    let Object hashMap = toJSON value
    in Object (hashMap |> HashMap.insert (pack "state") (toJSON (pack "Authenticated")))
  toJSON (AuthenticatedResponse failures value) =
    let Object hashMap = toJSON value
    in Object (hashMap
      |> HashMap.insert (pack "state") (toJSON (pack "Authenticated"))
      |> HashMap.insert (pack "failures") (toJSON failures))
  toJSON UnauthenticatedResponse = object ["state" .= pack "Unauthenticated"]

unauthenticatedResponse :: Response ()
unauthenticatedResponse = UnauthenticatedResponse

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
stripPrefix prefix =
  defaultOptions {
    fieldLabelModifier = lowercaseFirstCharacter . drop (length prefix)
  }
  where
  lowercaseFirstCharacter "" = ""
  lowercaseFirstCharacter (first : rest) = Char.toLower first : rest

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
