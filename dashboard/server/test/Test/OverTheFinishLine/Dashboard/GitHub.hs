{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.OverTheFinishLine.Dashboard.GitHub where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Identity
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeM)
import Test.Hspec
import Text.Heredoc

import qualified OverTheFinishLine.Dashboard.GitHub as GitHub
import OverTheFinishLine.Dashboard.Model

spec :: SpecWith ()
spec = describe "parsing of GitHub JSON" $ do
  it "parses the user JSON" $ do
    let expected = GitHub.User "47582" "SamirTalwar" "https://avatars.githubusercontent.com/u/47582?v=3"
    let actual = decode userJson
    actual `shouldBe` Just expected

  it "parses pull request JSON" $ do
    let expected = GitHub.PullRequest PullRequest {
      prRepository = Repository "sandwiches" "cheese" "https://github.com/sandwiches/cheese",
      prNumber = 123,
      prTitle = "Add support for French cheese.",
      prUpdatedAt = parseTime "2016-05-04T15:44:33Z",
      prUrl = "https://github.com/sandwiches/cheese/pull/123"
    }
    let actual = decode pullRequestJson
    actual `shouldBe` Just expected

parseTime :: String -> UTCTime
parseTime = runIdentity . parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Z"))

userJson :: ByteString
userJson =
  [str|{
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

pullRequestJson :: ByteString
pullRequestJson =
  [str|{
      |  "url": "https://api.github.com/repos/sandwiches/cheese/pulls/123",
      |  "id": 68884645,
      |  "html_url": "https://github.com/sandwiches/cheese/pull/123",
      |  "diff_url": "https://github.com/sandwiches/cheese/pull/123.diff",
      |  "patch_url": "https://github.com/sandwiches/cheese/pull/123.patch",
      |  "issue_url": "https://api.github.com/repos/sandwiches/cheese/issues/123",
      |  "number": 123,
      |  "state": "open",
      |  "locked": false,
      |  "title": "Add support for French cheese.",
      |  "user": {
      |    "login": "monsieur",
      |    "id": 1191970,
      |    "avatar_url": "https://avatars.githubusercontent.com/u/1191970?v=3",
      |    "gravatar_id": "",
      |    "url": "https://api.github.com/users/monsieur",
      |    "html_url": "https://github.com/monsieur"
      |  },
      |  "body": "As you know, French cheese is the best cheese.",
      |  "created_at": "2016-04-19T19:11:31Z",
      |  "updated_at": "2016-05-04T15:44:33Z",
      |  "closed_at": null,
      |  "merged_at": null,
      |  "merge_commit_sha": "37333f9e0fb66eb2e2d69839476cb3d10f3abf75",
      |  "assignee": null,
      |  "milestone": null,
      |  "head": {
      |    "label": "monsieur:french-cheese",
      |    "ref": "french-cheese",
      |    "repo": {
      |      "id": 56509514,
      |      "name": "cheese",
      |      "full_name": "monsieur/cheese",
      |      "owner": {
      |        "login": "monsieur",
      |        "id": 1191970
      |      },
      |      "html_url": "https://github.com/monsieur/cheese"
      |    }
      |  },
      |  "base": {
      |    "label": "sandwiches:master",
      |    "ref": "master",
      |    "repo": {
      |      "id": 25231002,
      |      "name": "cheese",
      |      "full_name": "sandwiches/cheese",
      |      "owner": {
      |        "login": "sandwiches",
      |        "id": 4359353
      |      },
      |      "html_url": "https://github.com/sandwiches/cheese"
      |    }
      |  }
      |}
      |]
