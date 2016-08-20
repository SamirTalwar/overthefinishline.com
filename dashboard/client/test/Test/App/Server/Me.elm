module Test.App.Server.Me exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Json.Decode exposing (decodeString)
import Task exposing (Task)
import Url

import App.Location as Location
import App.Model exposing (..)
import App.Server.Me exposing (..)

tests : Tests
tests =
  [
    test "App.Server.Me.decoder: decodes the user and projects" (
      let
        (_, decoder) = endpoint

        expected : Task String Me
        expected = Task.succeed (Me (User "_why" (GitHubAvatar <| Url.parse "https://example.com/avatars/_why.jpg")) [
          Project "Camping" (Location.Project (Url.parse "/projects/_why/camping")),
          Project "Hpricot" (Location.Project (Url.parse "/projects/_why/hpricot")),
          Project "RedCloth" (Location.Project (Url.parse "/projects/_why/redcloth")),
          Project "Shoes" (Location.Project (Url.parse "/project/_why/shoes"))
          ])

        actual : Task String Me
        actual = decodeString decoder meJson |> Task.fromResult
      in
        assert actual (equals expected)
    )
  ]

meJson : String
meJson =
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
