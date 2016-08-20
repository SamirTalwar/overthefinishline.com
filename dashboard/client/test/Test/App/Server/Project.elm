module Test.App.Server.Project exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Json.Decode exposing (decodeString)
import Task exposing (Task)
import Url exposing (Url)

import App.Model exposing (..)
import App.Server.Project exposing (..)

tests : Tests
tests =
  [
    test "App.Server.Project.decoder: decodes a project" (
      let
        expected : Task String Project
        expected = Task.succeed project

        actual : Task String Project
        actual = decodeString decoder projectJson |> Task.fromResult
      in
        assert actual (equals expected)
    )
  ]

project : Project
project =
  Project "elm-lang" "Elm" [
    { owner = "elm-lang", name = "core", url = Url.parse "https://github.com/elm-lang/core" },
    { owner = "elm-lang", name = "html", url = Url.parse "https://github.com/elm-lang/html" },
    { owner = "evancz", name = "elm-http", url = Url.parse "https://github.com/evancz/elm-http" }
  ]

projectJson : String
projectJson =
  """
    {
      "user": {
        "username": "elm-lang"
      },
      "name": "Elm",
      "repositories": [
        {"owner": "elm-lang", "name": "core", "url": "https://github.com/elm-lang/core"},
        {"owner": "elm-lang", "name": "html", "url": "https://github.com/elm-lang/html"},
        {"owner": "evancz", "name": "elm-http", "url": "https://github.com/evancz/elm-http"}
      ]
    }
  """
