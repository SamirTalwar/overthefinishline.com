module Test.App.Http exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Json.Decode exposing (..)
import Task exposing (Task)

import App.Http exposing (..)

tests : Tests
tests =
  [
    test "App.Http.get: verifies that the response is authenticated" (
      let
        expected : Task String (Response String)
        expected = Task.succeed authenticatedResponse

        actual : Task String (Response String)
        actual = decodeString (decoder ("thing" := string)) authenticatedJson |> Task.fromResult
      in
        assert actual (equals expected)
    ),

    test "App.Http.get: recognises an unauthenticated response" (
      let
        expected : Task String (Response Bool)
        expected = Task.succeed UnauthenticatedResponse

        actual : Task String (Response Bool)
        actual = decodeString (decoder (succeed True)) unauthenticatedJson |> Task.fromResult
      in
        assert actual (equals expected)
    )
  ]

authenticatedJson : String
authenticatedJson =
  """
    {
      "state": "Authenticated",
      "thing": "something"
    }
  """

authenticatedResponse : Response String
authenticatedResponse = Response "something"

unauthenticatedJson : String
unauthenticatedJson =
  """
    {
      "state": "Unauthenticated"
    }
  """
