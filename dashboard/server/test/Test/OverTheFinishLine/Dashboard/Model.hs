{-# LANGUAGE OverloadedStrings #-}

module Test.OverTheFinishLine.Dashboard.Model where

import Data.Aeson
import Data.Text (Text, pack)
import Test.Hspec

import OverTheFinishLine.Dashboard.Model

spec = describe "JSON serialization of the model" $ do
  it "serializes users" $ do
    let user = User "Steve" "https://example.com/avatars/Steve.jpg"
    let projects = [MyProject "Thing" "/projects/steve/thing" []]
    let me = Me user projects
    toJSON (AuthenticatedResponse me)
      `shouldBe` object [
          "state" .= pack "Authenticated",
          "user" .= object [
            "username" .= pack "Steve",
            "avatarUrl" .= pack "https://example.com/avatars/Steve.jpg"
          ],
          "projects" .= [
            object [
              "name" .= pack "Thing",
              "url" .= pack "/projects/steve/thing",
              "repositories" .= ([] :: [Text])
            ]
          ]
        ]

  it "serializes projects" $ do
    let repositories = ["ghc/ghc", "commercialhaskell/stack", "yesodweb/persistent"]
    let project = MyProject "Haskell" "/projects/me/Haskell" repositories
    toJSON (AuthenticatedResponse project)
      `shouldBe` object [
          "state" .= pack "Authenticated",
          "name" .= pack "Haskell",
          "url" .= pack "/projects/me/Haskell",
          "repositories" .= [
            pack "ghc/ghc",
            pack "commercialhaskell/stack",
            pack "yesodweb/persistent"
          ]
        ]

  it "serializes an unauthenticated response" $
    toJSON unauthenticatedResponse
      `shouldBe` object ["state" .= pack "Unauthenticated"]
