{-# LANGUAGE OverloadedStrings #-}

module OverTheFinishLine.Dashboard.GitHub where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text, pack)
import qualified Network.OAuth.OAuth2 as OAuth

data AuthenticatedUser = AuthenticatedUser {
  accessToken :: OAuth.AccessToken,
  user :: User
}

data User = User {
  id :: Text,
  login :: Text
}

instance FromJSON User where
  parseJSON (Object v) =
    User
      <$> (numberToString <$> v .: "id")
      <*> (v .: "login")
  parseJSON _ = mzero

numberToString :: Integer -> Text
numberToString = pack . show
