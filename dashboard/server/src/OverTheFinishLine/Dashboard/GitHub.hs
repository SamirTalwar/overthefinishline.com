{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module OverTheFinishLine.Dashboard.GitHub (
  User (..),
  PullRequest (..),
  Repository (..),
  Statuses (..)
) where

import Control.Monad ((<=<), MonadPlus, mzero)
import Data.Aeson
import Data.Text (Text, pack)
import qualified Data.Vector as Vector
import GHC.Generics

import qualified OverTheFinishLine.Dashboard.Model as Model
import OverTheFinishLine.Dashboard.Lists

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

data PullRequest = PullRequest { unPullRequest :: Model.PullRequest, prStatusesUrl :: Model.Url }
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
      <*> v .: "statuses_url"
  parseJSON _ = mzero

newtype Statuses = Statuses { unStatuses :: [Model.StatusContext] }
  deriving (Eq, Show, Generic)

instance FromJSON Statuses where
  parseJSON (Array v) = do
    statusObjects <- mapM toObject (Vector.toList v)
    statuses <- liftedGroupQueryBy Model.StatusContext (.: "context") (parseState <=< (.: "state")) statusObjects
    return $ Statuses statuses
  parseJSON _ = mzero

parseState :: MonadPlus m => Text -> m Model.ItemStatus
parseState "error" = return Model.StatusFailure
parseState "pending" = return Model.StatusPending
parseState "success" = return Model.StatusSuccess
parseState _ = mzero

numberToText :: Integer -> Text
numberToText = pack . show

toObject :: MonadPlus m => Value -> m Object
toObject (Object v) = return v
toObject _ = mzero
