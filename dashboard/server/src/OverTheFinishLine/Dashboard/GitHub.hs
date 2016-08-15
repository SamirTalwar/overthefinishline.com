{-# LANGUAGE OverloadedStrings #-}

module OverTheFinishLine.Dashboard.GitHub (GitHubUser (..)) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text, pack)

import OverTheFinishLine.Dashboard.Model

data GitHubUser =
  GitHubUser {
    gitHubUserId :: Text,
    gitHubUserLogin :: Text,
    gitHubUserAvatarUrl :: Text
  }

instance FromJSON GitHubUser where
  parseJSON (Object v) =
    GitHubUser
      <$> (numberToText <$> v .: "id")
      <*> (v .: "login")
      <*> (v .: "avatar_url")
  parseJSON _ = mzero

numberToText :: Integer -> Text
numberToText = pack . show
