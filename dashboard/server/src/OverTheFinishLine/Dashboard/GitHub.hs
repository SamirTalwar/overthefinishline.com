{-# LANGUAGE OverloadedStrings #-}

module OverTheFinishLine.Dashboard.GitHub (GitHubUser (..)) where

import Control.Monad ((<=<), foldM, mzero)
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, pack)

import OverTheFinishLine.Dashboard.Model

data GitHubUser =
  GitHubUser {
    gitHubUserId :: Text,
    gitHubUserLogin :: Text,
    gitHubUserAvatarUrl :: Text
  }
  deriving (Eq, Show)

instance FromJSON GitHubUser where
  parseJSON (Object v) =
    GitHubUser
      <$> (numberToText <$> v .: "id")
      <*> v .: "login"
      <*> v .: "avatar_url"
  parseJSON _ = mzero

instance FromJSON PullRequest where
  parseJSON (Object v) =
    PullRequest
      <$> (v .: "base" >>= (.: "repo"))
      <*> v .: "number"
      <*> v .: "title"
      <*> v .: "updated_at"
      <*> v .: "html_url"
  parseJSON _ = mzero

instance FromJSON Repository where
  parseJSON (Object v) =
    Repository
      <$> (v .: "owner" >>= (.: "login"))
      <*> v .: "name"
      <*> v .: "html_url"
  parseJSON _ = mzero

numberToText :: Integer -> Text
numberToText = pack . show
