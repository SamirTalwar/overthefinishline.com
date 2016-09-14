{-# LANGUAGE OverloadedStrings #-}

module Test.OverTheFinishLine.Dashboard.Model where

import Data.Aeson
import Data.Functor.Identity (runIdentity)
import Data.Text (Text, pack)
import Data.Time.Format
import Test.Hspec

import OverTheFinishLine.Dashboard.Model

spec :: SpecWith ()
spec = describe "JSON serialization of the model" $ do
  it "serializes users" $ do
    let user = User "Steve" "https://example.com/avatars/Steve.jpg"
    let projects = [MyProject "Thing" "/projects/steve/thing" []]
    let me = Me user projects
    toJSON (AuthenticatedResponse [] me)
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
    toJSON (AuthenticatedResponse [] project)
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

  it "serializes PRs with failures" $ do
    let repository = Repository "commercialhaskell" "stack" "https://github.com/commercialhaskell/stack"
    let timestamp = runIdentity $ parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) "2016-01-01"
    let pullRequests = [PullRequest repository 32 "Something" timestamp "https://github.com/commercialhaskell/stack/pulls/32"]
    let dashboard = Dashboard timestamp pullRequests
    let failures = [RequestFailure "https://api.github.com/repos/ghc/ghc/pulls" "Request failed."]

    let timestampJSON = pack "2016-01-01T00:00:00Z"
    let repositoryJSON = object ["owner" .= pack "commercialhaskell",
                                 "name" .= pack "stack",
                                 "url" .= pack "https://github.com/commercialhaskell/stack"]
    toJSON (AuthenticatedResponse failures dashboard)
      `shouldBe` object [
          "state" .= pack "Authenticated",
          "now" .= timestampJSON,
          "pullRequests" .= [
            object [
              "repository" .= repositoryJSON,
              "number" .= (32 :: Int),
              "title" .= pack "Something",
              "updatedAt" .= timestampJSON,
              "url" .= pack "https://github.com/commercialhaskell/stack/pulls/32"
            ]
          ],
          "failures" .= [
            object [
              "tag" .= pack "RequestFailure",
              "url" .= pack "https://api.github.com/repos/ghc/ghc/pulls",
              "message" .= pack "Request failed."
            ]
          ]
        ]

  it "serializes an unauthenticated response" $
    toJSON unauthenticatedResponse
      `shouldBe` object ["state" .= pack "Unauthenticated"]
