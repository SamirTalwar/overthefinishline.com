{-# LANGUAGE OverloadedStrings #-}

module OverTheFinishLine.Dashboard.GitHub (GitHubUser (..)) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text, pack)

import OverTheFinishLine.Dashboard.Model

data GitHubUser = GitHubUser {
  userId :: Text,
  userLogin :: Text
}

instance FromJSON GitHubUser where
  parseJSON (Object v) =
    GitHubUser
      <$> (numberToText <$> v .: "id")
      <*> (v .: "login")
  parseJSON _ = mzero

numberToText :: Integer -> Text
numberToText = pack . show
