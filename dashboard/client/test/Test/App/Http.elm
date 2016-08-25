module Test.App.Http exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Dict
import Json.Decode exposing (..)
import String
import Task exposing (Task)
import Url

import App.Error as Error exposing (Error)
import App.Http exposing (..)
import App.Location as Location exposing (Location)
import Http

tests : Tests
tests =
  [
    test "App.Http.get: verifies that the response is authenticated" (
      let
        expected : Task Error (Response String)
        expected = Task.succeed authenticatedResponse

        request = {
          verb = "GET",
          headers = [("Accept", "application/json")],
          url = "/me",
          body = Http.empty
        }
        response = {
          status = 200,
          statusText = "OK",
          headers = Dict.empty,
          url = "/me",
          value = Http.Text authenticatedJson
        }
        send = stubSend request response

        actual : Task Error (Response String)
        actual = get' send Location.Me ("thing" := string)
      in
        assert actual (equals expected)
    ),

    test "App.Http.get: recognises an unauthenticated response" (
      let
        expected : Task Error (Response Bool)
        expected = Task.succeed UnauthenticatedResponse

        request = {
          verb = "GET",
          headers = [("Accept", "application/json")],
          url = "/me",
          body = Http.empty
        }
        response = {
          status = 401,
          statusText = "Unauthorized",
          headers = Dict.empty,
          url = "/me",
          value = Http.Text unauthenticatedJson
        }
        send = stubSend request response

        actual : Task Error (Response Bool)
        actual = get' send Location.Me (succeed True)
      in
        assert actual (equals expected)
    ),

    test "App.Http.get: extracts partial failures from the response" (
      let
        expected : Task Error (Response String)
        expected = Task.succeed responseWithFailures

        request = {
          verb = "GET",
          headers = [("Accept", "application/json")],
          url = "/me",
          body = Http.empty
        }
        response = {
          status = 200,
          statusText = "OK",
          headers = Dict.empty,
          url = "/me",
          value = Http.Text responseWithFailuresJson
        }
        send = stubSend request response

        actual : Task Error (Response String)
        actual = get' send Location.Me ("thing" := string)
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
authenticatedResponse = Response [] "something"

unauthenticatedJson : String
unauthenticatedJson =
  """
    {
      "state": "Unauthenticated"
    }
  """

responseWithFailuresJson : String
responseWithFailuresJson =
  """
    {
      "state": "Authenticated",
      "failures": [
        {
          "tag": "RequestFailure",
          "url": "https://example.com/foo",
          "message": "Not found at all, mate."
        }
      ],
      "thing": "something else"
    }
  """

responseWithFailures : Response String
responseWithFailures = Response [RequestFailure (Url.parse "https://example.com/foo") "Not found at all, mate."] "something else"

stubSend : Http.Request -> Http.Response -> Send
stubSend expectedRequest response actualRequest =
  assert (Task.succeed actualRequest) (equals (Task.succeed expectedRequest))
    |> Task.map (always response)
    |> (flip Task.onError) (\failureMessages ->
      Task.succeed {
        status = 500,
        statusText = "Test Failed",
        headers = Dict.empty,
        url = actualRequest.url,
        value = Http.Text (failureMessages |> List.map (\(key, value) -> key ++ ": " ++ value) |> String.join "\n")
      })
