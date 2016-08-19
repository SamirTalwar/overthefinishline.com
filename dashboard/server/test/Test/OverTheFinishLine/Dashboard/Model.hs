{-# LANGUAGE OverloadedStrings #-}

module Test.OverTheFinishLine.Dashboard.Model where

import Data.Aeson
import Data.Text (pack)
import Test.Hspec

import OverTheFinishLine.Dashboard.Model

spec = describe "JSON serialization of the model" $ do
  it "serializes authenticated users" $ do
    let value = UserProjects (User "Steve" "https://example.com/avatars/Steve.jpg") [UserProject "Thing" "/projects/steve/thing"]
    toJSON (AuthenticatedResponse value)
      `shouldBe` object [
          "state" .= pack "Authenticated",
          "user" .= object [
            "username" .= pack "Steve",
            "avatarUrl" .= pack "https://example.com/avatars/Steve.jpg"
          ],
          "projects" .= [
            object [
              "name" .= pack "Thing",
              "url" .= pack "/projects/steve/thing"
            ]
          ]
        ]

  it "serializes an unauthenticated response" $
    toJSON unauthenticatedResponse
      `shouldBe` object ["state" .= pack "Unauthenticated"]
