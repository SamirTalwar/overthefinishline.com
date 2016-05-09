module Test.GitHub.PullRequests where

import Http
import Json.Decode exposing (decodeString)
import Task
import TestFramework exposing (test)

import GitHub.PullRequests exposing (..)

tests =
  [
    test "GitHub.PullRequests.fetch: fetches the pull requests and decodes the JSON" (
      let
        get mapping url =
          if url == Http.url "https://api.github.com/repos/sandwiches/cheese/pulls" []
            then Task.fromResult (decodeString mapping cheeseSandwichPullRequestJson)
            else Task.fail "Not Found"
        expected = Task.succeed [
          {title = "Add support for French cheese."},
          {title = "Discontinue pre-sliced cheese wrapped in plastic."}
        ]
        actual = fetch get {owner = "sandwiches", repository = "cheese"}
      in
        Task.map2 (==) expected actual
    )
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
      "milestone": null
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
      "milestone": null
    }
  ]
  """
