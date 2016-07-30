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
    UserIsUnauthenticated
  | MissingAuthenticationCode
  | InvalidAuthenticationCode Text
  | QueryFailure Text

data Model =
    Unauthenticated
  | Dashboard {
      modelNow :: UTCTime,
      modelLogin :: String,
      modelPullRequests :: [PullRequest]
    }
  deriving (Generic, Show)
instance ToJSON Model where toJSON = genericToJSON (stripPrefix "model")

data PullRequest = PullRequest {
  prRepository :: Repository,
  prNumber :: Int,
  prTitle :: String,
  prUpdatedAt :: UTCTime,
  prLink :: Link
}
  deriving (Generic, Show)
instance ToJSON PullRequest where toJSON = genericToJSON (stripPrefix "pr")

data Repository = Repository {
  repoOwner :: String,
  repoName :: String,
  repoLink :: Link
}
  deriving (Generic, Show)
instance ToJSON Repository where toJSON = genericToJSON (stripPrefix "repo")

type Link = String

stripPrefix :: String -> Options
stripPrefix prefix = defaultOptions { fieldLabelModifier = stripPrefixFromFields prefix }
  where
  stripPrefixFromFields prefix = lowercaseFirstCharacter . drop (length prefix)
  lowercaseFirstCharacter (first : rest) = Char.toLower first : rest
