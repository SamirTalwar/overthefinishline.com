module Test.GitHub.PullRequests (tests) where

import Http
import Json.Decode exposing (Decoder, decodeString)
import Moment
import Task exposing (Task)
import TestFramework exposing (test)

import Error exposing (Error)
import GitHub.PullRequests exposing (..)
import Model exposing (..)

tests =
  [
    test "GitHub.PullRequests.fetch: fetches the pull requests" (
      let
        get : Decoder a -> String -> Task Http.Error a
        get decoder url =
          if url == "https://api.github.com/repos/sandwiches/cheese/pulls"
            then Task.fromResult (decodeString decoder cheeseSandwichPullRequestJson)
                   |> Task.mapError Http.UnexpectedPayload
            else Task.fail (Http.BadResponse 404 "Not Found")

        expected : Task Error PullRequests
        expected = pullRequests |> Result.formatError (\_ -> Error.UnexpectedResponse) |> Task.fromResult

        actual : Task Error PullRequests
        actual = fetch get {owner = "sandwiches", repository = "cheese"}
      in
        (Task.map2 (==) expected actual |> Task.mapError toString,
         [("Expected", toString expected), ("Actual", toString actual)])
    )
  ]

pullRequests =
  Model.pullRequests [
    {
      repository = {owner = "sandwiches", repository = "cheese"},
      number = 123,
      title = "Add support for French cheese.",
      updatedAt = Moment.parse "2016-05-04T15:44:33Z"
    },
    {
      repository = {owner = "sandwiches", repository = "cheese"},
      number = 121,
      title = "Discontinue pre-sliced cheese wrapped in plastic.",
      updatedAt = Moment.parse "2016-02-06T03:08:56Z"
    }
  ]

cheeseSandwichPullRequestJson =
  """
  [
    {
      "url": "https://api.github.com/repos/sandwiches/cheese/pulls/123",
      "id": 68884645,
      "html_url": "https://github.com/sandwiches/cheese/pull/123",
      "diff_url": "https://github.com/sandwiches/cheese/pull/123.diff",
      "patch_url": "https://github.com/sandwiches/cheese/pull/123.patch",
      "issue_url": "https://api.github.com/repos/sandwiches/cheese/issues/123",
      "number": 123,
      "state": "open",
      "locked": false,
      "title": "Add support for French cheese.",
      "user": {
        "login": "monsieur",
        "id": 1191970,
        "avatar_url": "https://avatars.githubusercontent.com/u/1191970?v=3",
        "gravatar_id": "",
        "url": "https://api.github.com/users/monsieur",
        "html_url": "https://github.com/monsieur"
      },
      "body": "As you know, French cheese is the best cheese.",
      "created_at": "2016-04-19T19:11:31Z",
      "updated_at": "2016-05-04T15:44:33Z",
      "closed_at": null,
      "merged_at": null,
      "merge_commit_sha": "37333f9e0fb66eb2e2d69839476cb3d10f3abf75",
      "assignee": null,
      "milestone": null,
      "head": {
        "label": "monsieur:french-cheese",
        "ref": "french-cheese",
        "repo": {
          "id": 56509514,
          "name": "cheese",
          "full_name": "monsieur/cheese",
          "owner": {
            "login": "monsieur",
            "id": 1191970
          }
        }
      },
      "base": {
        "label": "sandwiches:master",
        "ref": "master",
        "repo": {
          "id": 25231002,
          "name": "cheese",
          "full_name": "sandwiches/cheese",
          "owner": {
            "login": "sandwiches",
            "id": 4359353
          }
        }
      }
    },
    {
      "url": "https://api.github.com/repos/sandwiches/cheese/pulls/121",
      "id": 68881294,
      "html_url": "https://github.com/sandwiches/cheese/pull/121",
      "diff_url": "https://github.com/sandwiches/cheese/pull/121.diff",
      "patch_url": "https://github.com/sandwiches/cheese/pull/121.patch",
      "issue_url": "https://api.github.com/repos/sandwiches/cheese/issues/121",
      "number": 121,
      "state": "open",
      "locked": false,
      "title": "Discontinue pre-sliced cheese wrapped in plastic.",
      "user": {
        "login": "eco",
        "id": 39174867,
        "avatar_url": "https://avatars.githubusercontent.com/u/39174867?v=3",
        "gravatar_id": "",
        "url": "https://api.github.com/users/eco",
        "html_url": "https://github.com/eco"
      },
      "body": "This stuff is not cheese. BTW, I will be attacking canned spray cheese next.",
      "created_at": "2016-01-29T09:47:21Z",
      "updated_at": "2016-02-06T03:08:56Z",
      "closed_at": null,
      "merged_at": null,
      "merge_commit_sha": "7c75d234c2a9b8bf61275224675ab5350d05e413",
      "assignee": null,
      "milestone": null,
      "head": {
        "label": "eco:no-plastic",
        "ref": "no-plastic",
        "repo": {
          "id": 28364877,
          "name": "cheese",
          "full_name": "eco/cheese",
          "owner": {
            "login": "eco",
            "id": 68881294
          }
        }
      },
      "base": {
        "label": "sandwiches:master",
        "ref": "master",
        "repo": {
          "id": 25231002,
          "name": "cheese",
          "full_name": "sandwiches/cheese",
          "owner": {
            "login": "sandwiches",
            "id": 4359353
          }
        }
      }
    }
  ]
  """
