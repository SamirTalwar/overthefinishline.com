module Test.Server.Dashboard exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeString)
import Moment exposing (Moment)
import Task exposing (Task)

import Server.Dashboard exposing (..)
import Error exposing (Error)
import Model exposing (..)

tests : Tests
tests =
  [
    test "Server.Dashboard.fetch: fetches a dashboard full of pull requests" (
      let
        get : Decoder a -> String -> Task Http.Error a
        get decoder url =
          if url == "/dashboard"
            then Task.fromResult (decodeString decoder dashboardJson)
                   |> Task.mapError Http.UnexpectedPayload
            else Task.fail (Http.BadResponse 404 "Not Found")

        expected : Task Error Dashboard
        expected = dashboard |> Result.formatError Error.UnexpectedResponse |> Task.fromResult

        actual : Task Error Dashboard
        actual = fetch get
      in
        assert actual (equals expected)
    )
  ]

dashboard : Result String Dashboard
dashboard =
  dashboardResult {
    now = Moment.parse "2016-06-01T08:00:00Z",
    pullRequests = [
      {
        repository = {
          owner = "sandwiches",
          name = "cheese",
          link = "https://github.com/sandwiches/cheese"
        },
        number = 123,
        title = "Add support for French cheese.",
        updatedAt = Moment.parse "2016-05-04T15:44:33Z",
        link = "https://github.com/sandwiches/cheese/pull/123"
      },
      {
        repository = {
          owner = "sandwiches",
          name = "cheese",
          link = "https://github.com/sandwiches/cheese"
        },
        number = 121,
        title = "Discontinue pre-sliced cheese wrapped in plastic.",
        updatedAt = Moment.parse "2016-02-06T03:08:56Z",
        link = "https://github.com/sandwiches/cheese/pull/121"
      }
    ]
  }

dashboardJson =
  """
    {
      "tag": "Dashboard",
      "now": "2016-06-01T08:00:00Z",
      "pullRequests": [
        {
          "repository": {
            "owner": "sandwiches",
            "name": "cheese",
            "link": "https://github.com/sandwiches/cheese"
          },
          "number": 123,
          "title": "Add support for French cheese.",
          "updatedAt": "2016-05-04T15:44:33Z",
          "link": "https://github.com/sandwiches/cheese/pull/123"
        },
        {
          "repository": {
            "owner": "sandwiches",
            "name": "cheese",
            "link": "https://github.com/sandwiches/cheese"
          },
          "number": 121,
          "title": "Discontinue pre-sliced cheese wrapped in plastic.",
          "updatedAt": "2016-02-06T03:08:56Z",
          "link": "https://github.com/sandwiches/cheese/pull/121"
        }
      ]
    }
  """

dashboardResult
    : { now: Result String Moment,
        pullRequests: List { repository : Repository, number : Int, title : String, updatedAt : Result String Moment, link : Link } }
    -> Result String Dashboard
dashboardResult {now, pullRequests} =
  let
    pullRequestsResult = pullRequests
      |> List.map (\record ->
           case record.updatedAt of
             Ok updatedAtMoment -> Ok { record | updatedAt = updatedAtMoment }
             Err error -> Err error)
      |> sequenceResults
  in
    Result.map2 Dashboard now pullRequestsResult

sequenceResults : List (Result a b) -> Result a (List b)
sequenceResults = List.foldr (Result.map2 (::)) (Ok [])
