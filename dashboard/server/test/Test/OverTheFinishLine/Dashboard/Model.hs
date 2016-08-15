{-# LANGUAGE OverloadedStrings #-}

module Test.OverTheFinishLine.Dashboard.Model where

import Data.Aeson
import Data.Text (pack)
import Test.Hspec

import OverTheFinishLine.Dashboard.Model

spec = describe "JSON serialization of the model" $ do
  it "serializes authenticated users" $
    toJSON (AuthenticatedResponse (User "Steve" [Project "Thing"]))
      `shouldBe` object ["state" .= pack "Authenticated",
                         "username" .= pack "Steve",
                         "projects" .= [object ["name" .= pack "Thing"]]]

  it "serializes an unauthenticated response" $
    toJSON unauthenticatedResponse
      `shouldBe` object ["state" .= pack "Unauthenticated"]
