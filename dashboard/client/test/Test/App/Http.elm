module Test.App.Http exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Json.Decode exposing (..)
import Task exposing (Task)
import Test.Helpers.Task exposing (taskFromResult)
import Url
import App.Http as Http
import App.Model exposing (..)


tests : Tests
tests =
    [ test "App.Http.decoder: verifies that the response is authenticated"
        (let
            expected =
                Task.succeed authenticatedResponse

            actual =
                decodeString (Http.decoder (field "thing" string)) authenticatedJson |> taskFromResult
         in
            assert actual (equals expected)
        )
    , test "App.Http.get: recognises an unauthenticated response"
        (let
            expected =
                Task.succeed UnauthenticatedResponse

            actual =
                decodeString (Http.decoder (succeed True)) unauthenticatedJson |> taskFromResult
         in
            assert actual (equals expected)
        )
    , test "App.Http.get: extracts partial failures from the response"
        (let
            expected =
                Task.succeed responseWithFailures

            actual =
                decodeString (Http.decoder (field "thing" string)) responseWithFailuresJson |> taskFromResult
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
authenticatedResponse =
    Response [] "something"


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
responseWithFailures =
    Response [ RequestFailure (Url.parse "https://example.com/foo") "Not found at all, mate." ] "something else"
