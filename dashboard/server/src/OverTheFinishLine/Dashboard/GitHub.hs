{-# LANGUAGE OverloadedStrings #-}

module OverTheFinishLine.Dashboard.GitHub (User (..)) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text, pack)

import OverTheFinishLine.Dashboard.Model

data User = User {
  userId :: Text,
  userLogin :: Text
}

instance FromJSON User where
  parseJSON (Object v) =
    User
      <$> (numberToText <$> v .: "id")
      <*> (v .: "login")
  parseJSON _ = mzero

numberToText :: Integer -> Text
numberToText = pack . show
