{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.OverTheFinishLine.Dashboard.GitHub where

import Data.Aeson
import Data.Text (pack)
import Test.Hspec
import Text.Heredoc

import OverTheFinishLine.Dashboard.GitHub

spec = describe "parsing of GitHub JSON" $ do
  it "parses the user JSON" $ do
    let json = [str|{
                   |  "login": "SamirTalwar",
                   |  "id": 47582,
                   |  "avatar_url": "https://avatars.githubusercontent.com/u/47582?v=3",
                   |  "gravatar_id": "",
                   |  "url": "https://api.github.com/users/SamirTalwar",
                   |  "html_url": "https://github.com/SamirTalwar",
                   |  "followers_url": "https://api.github.com/users/SamirTalwar/followers",
                   |  "following_url": "https://api.github.com/users/SamirTalwar/following{/other_user}",
                   |  "gists_url": "https://api.github.com/users/SamirTalwar/gists{/gist_id}",
                   |  "starred_url": "https://api.github.com/users/SamirTalwar/starred{/owner}{/repo}",
                   |  "subscriptions_url": "https://api.github.com/users/SamirTalwar/subscriptions",
                   |  "organizations_url": "https://api.github.com/users/SamirTalwar/orgs",
                   |  "repos_url": "https://api.github.com/users/SamirTalwar/repos",
                   |  "events_url": "https://api.github.com/users/SamirTalwar/events{/privacy}",
                   |  "received_events_url": "https://api.github.com/users/SamirTalwar/received_events",
                   |  "type": "User",
                   |  "site_admin": false,
                   |  "name": "Samir Talwar",
                   |  "company": "Noodle Sandwich",
                   |  "blog": "http://samirtalwar.com/",
                   |  "location": "London, UK",
                   |  "email": "samir@noodlesandwich.com",
                   |  "hireable": null,
                   |  "bio": "Unless otherwise specified, all my repositories are bound by the Contributor Covenant (http://contributor-covenant.org/version/1/4/).",
                   |  "public_repos": 57,
                   |  "public_gists": 39,
                   |  "followers": 110,
                   |  "following": 21,
                   |  "created_at": "2009-01-19T05:38:21Z",
                   |  "updated_at": "2016-08-15T22:00:58Z"
                   |}
                   |]
    let parsed = decode json
    parsed `shouldBe` Just (GitHubUser "47582" "SamirTalwar" "https://avatars.githubusercontent.com/u/47582?v=3")
