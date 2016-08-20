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
        expected : Task String Me
        expected =
          Task.succeed (Me (User "_why" (GitHubAvatar <| Url.parse "https://example.com/avatars/_why.jpg")) [
            Project "_why" "Camping",
            Project "_why" "Hpricot",
            Project "_why" "RedCloth",
            Project "_why" "Shoes"
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
