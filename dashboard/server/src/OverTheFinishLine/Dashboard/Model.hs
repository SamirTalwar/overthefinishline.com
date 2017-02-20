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
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Database.Persist.Class (PersistEntity, keyToValues)
import Database.Persist.TH
import Database.Persist.Types
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
    deriving Eq Generic Show

  ProjectRepository
    projectId ProjectId
    name Text
    UniqueProjectRepositoryNameByProject projectId name
    deriving Eq Show
|]

instance Hashable PersistValue where
  s `hashWithSalt` (PersistText text) = s `hashWithSalt` text
  s `hashWithSalt` (PersistByteString byteString) = s `hashWithSalt` byteString
  s `hashWithSalt` (PersistInt64 int) = s `hashWithSalt` int
  _ `hashWithSalt` value = error ("I can't persist " ++ show value)

instance PersistEntity a => Hashable (Key a) where
  s `hashWithSalt` key = s `hashWithSalt` keyToValues key

instance Hashable Project

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
    dashboardPullRequests :: [PullRequestWithStatus]
  }
  deriving (Eq, Generic, Show)
instance ToJSON Dashboard where toJSON = genericToJSON (stripPrefix "dashboard")

data Repository =
  Repository {
    repoOwner :: Text,
    repoName :: Text,
    repoUrl :: Url
  }
  deriving (Eq, Generic, Show)
instance ToJSON Repository where toJSON = genericToJSON (stripPrefix "repo")

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

data PullRequestWithStatus =
  PullRequestWithStatus {
    prsRepository :: Repository,
    prsNumber :: Int,
    prsTitle :: Text,
    prsStatus :: ItemStatus,
    prsUpdatedAt :: UTCTime,
    prsUrl :: Url
  }
  deriving (Eq, Generic, Show)
instance ToJSON PullRequestWithStatus where toJSON = genericToJSON (stripPrefix "prs")

data StatusContext =
  StatusContext {
    statusContext :: String,
    statusHistory :: [ItemStatus]
  }
  deriving (Eq, Generic, Show)
instance ToJSON StatusContext where toJSON = genericToJSON (stripPrefix "status")

data ItemStatus = StatusFailure | StatusPending | StatusSuccess | StatusNone
  deriving (Eq, Ord, Generic, Show)
instance ToJSON ItemStatus where toJSON = genericToJSON (taggedUnionWithPrefix "Status")

singleStatus :: [StatusContext] -> ItemStatus
singleStatus = fromMaybe StatusNone . listToMaybe . sort . map (head . statusHistory)

projectUrl :: User -> Project -> Url
projectUrl user project =
  decodeUtf8 $ toStrict $ toLazyByteString
    $ encodePathSegments ["projects", userUsername user, projectName project]

stripPrefix :: String -> Options
stripPrefix prefix =
  defaultOptions {
    fieldLabelModifier = lowercaseFirstCharacter . drop (length prefix)
  }

taggedUnionWithPrefix :: String -> Options
taggedUnionWithPrefix prefix =
  defaultOptions {
    constructorTagModifier = lowercaseFirstCharacter . drop (length prefix),
    sumEncoding = UntaggedValue
  }

lowercaseFirstCharacter :: String -> String
lowercaseFirstCharacter "" = ""
lowercaseFirstCharacter (first : rest) = Char.toLower first : rest

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
