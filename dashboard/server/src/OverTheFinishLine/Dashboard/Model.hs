{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OverTheFinishLine.Dashboard.Model where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Char as Char
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import GHC.Generics

data Exception =
    Unauthenticated
  | MissingUser
  | MissingAuthenticationCode
  | InvalidAuthenticationCode Text
  | QueryFailure Text

data ThirdPartyService = GitHub
  deriving (Eq, Read, Show)

data UserProjects =
    UnauthenticatedUser
  | AuthenticatedUser {
      userUsername :: Text,
      userProjects :: [Project]
    }
  deriving (Generic, Show)
instance ToJSON UserProjects where toJSON = genericToJSON (stripPrefix "user")

data Project = Project
  deriving (Generic, Show)
instance ToJSON Project where toJSON = genericToJSON (stripPrefix "project")

data Dashboard =
  Dashboard {
    dashboardNow :: UTCTime,
    dashboardPullRequests :: [PullRequest]
  }
  deriving (Generic, Show)
instance ToJSON Dashboard where toJSON = genericToJSON (stripPrefix "dashboard")

data PullRequest = PullRequest {
  prRepository :: Repository,
  prNumber :: Int,
  prTitle :: Text,
  prUpdatedAt :: UTCTime,
  prLink :: Link
}
  deriving (Generic, Show)
instance ToJSON PullRequest where toJSON = genericToJSON (stripPrefix "pr")

data Repository = Repository {
  repoOwner :: Text,
  repoName :: Text,
  repoLink :: Link
}
  deriving (Generic, Show)
instance ToJSON Repository where toJSON = genericToJSON (stripPrefix "repo")

type Link = Text

stripPrefix :: String -> Options
stripPrefix prefix = defaultOptions { fieldLabelModifier = stripPrefixFromFields prefix }
  where
  stripPrefixFromFields prefix = lowercaseFirstCharacter . drop (length prefix)
  lowercaseFirstCharacter (first : rest) = Char.toLower first : rest
