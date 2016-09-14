{-# LANGUAGE OverloadedStrings #-}

module OverTheFinishLine.Dashboard.GitHub (
  User (..),
  PullRequest (..),
  Repository (..)
) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text, pack)

import qualified OverTheFinishLine.Dashboard.Model as Model

data User =
  User {
    gitHubUserId :: Text,
    gitHubUserLogin :: Text,
    gitHubUserAvatarUrl :: Text
  }
  deriving (Eq, Show)

instance FromJSON User where
  parseJSON (Object v) =
    User
      <$> (numberToText <$> v .: "id")
      <*> v .: "login"
      <*> v .: "avatar_url"
  parseJSON _ = mzero

newtype PullRequest = PullRequest { unPullRequest :: Model.PullRequest }
  deriving (Eq, Show)

instance FromJSON PullRequest where
  parseJSON (Object v) =
    PullRequest
      <$> (Model.PullRequest
        <$> (unRepository <$> (v .: "base" >>= (.: "repo")))
        <*> v .: "number"
        <*> v .: "title"
        <*> v .: "updated_at"
        <*> v .: "html_url")
  parseJSON _ = mzero

newtype Repository = Repository { unRepository :: Model.Repository }
  deriving (Eq, Show)

instance FromJSON Repository where
  parseJSON (Object v) =
    Repository
      <$> (Model.Repository
        <$> (v .: "owner" >>= (.: "login"))
        <*> v .: "name"
        <*> v .: "html_url")
  parseJSON _ = mzero

numberToText :: Integer -> Text
numberToText = pack . show
