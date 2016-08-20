module Test.App.Server.Me exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Task exposing (Task)
import Url

import App.Http exposing (..)
import App.Error exposing (..)
import App.Location as Location
import App.Model exposing (..)
import App.Server.Me exposing (..)

tests : Tests
tests =
  [
    test "App.Server.Me.fetch: fetches the user" (
      let
        expected : Task Error (Response Me)
        expected = Task.succeed (Response (Me (User "_why" (GitHubAvatar <| Url.parse "https://example.com/avatars/_why.jpg")) [
          Project "Camping" (Location.Project (Url.parse "/projects/_why/camping")),
          Project "Hpricot" (Location.Project (Url.parse "/projects/_why/hpricot")),
          Project "RedCloth" (Location.Project (Url.parse "/projects/_why/redcloth")),
          Project "Shoes" (Location.Project (Url.parse "/project/_why/shoes"))
        ]))

        actual : Task Error (Response Me)
        actual = fetch (stubGet "/me" authenticatedUserJson)
      in
        assert actual (equals expected)
    ),

    test "App.Server.Me.fetch: recognises an unauthenticated response" (
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
          { "name": "Camping", "url": "/projects/_why/camping" },
          { "name": "Hpricot", "url": "/projects/_why/hpricot" },
          { "name": "RedCloth", "url": "/projects/_why/redcloth" },
          { "name": "Shoes", "url": "/project/_why/shoes" }
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
