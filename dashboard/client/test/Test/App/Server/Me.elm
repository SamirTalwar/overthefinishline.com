module Test.Server.Me exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Task exposing (Task)
import Url

import App.Http exposing (..)
import App.Error exposing (..)
import App.Model exposing (..)
import App.Server.Me exposing (..)

tests : Tests
tests =
  [
    test "Server.Me.fetch: fetches the user" (
      let
        expected : Task Error (Response Me)
        expected = Task.succeed (Response (Me (User "_why" (GitHubAvatar <| Url.parse "https://example.com/avatars/_why.jpg")) [
          Project "Camping" (Url.parse "/projects/camping"),
          Project "Hpricot" (Url.parse "/projects/hpricot"),
          Project "RedCloth" (Url.parse "/projects/redcloth"),
          Project "Shoes" (Url.parse "/project/shoes")
        ]))

        actual : Task Error (Response Me)
        actual = fetch (stubGet "/me" authenticatedUserJson)
      in
        assert actual (equals expected)
    ),

    test "Server.Me.fetch: recognises an unauthenticated response" (
      let
        expected : Task Error (Response Me)
        expected = Task.succeed UnauthenticatedResponse

        actual : Task Error (Response Me)
        actual = fetch (stubGet "/me" unauthenticatedJson)
      in
        assert actual (equals expected)
    )
  ]

authenticatedUserJson : String
authenticatedUserJson =
  """
    {
      "state": "Authenticated",
      "user": {
        "username": "_why",
        "avatarUrl": "https://example.com/avatars/_why.jpg"
      },
      "projects": [
          { "name": "Camping", "url": "/projects/camping" },
          { "name": "Hpricot", "url": "/projects/hpricot" },
          { "name": "RedCloth", "url": "/projects/redcloth" },
          { "name": "Shoes", "url": "/project/shoes" }
        ]
    }
  """

unauthenticatedJson : String
unauthenticatedJson =
  """
    {
      "state": "Unauthenticated"
    }
  """
