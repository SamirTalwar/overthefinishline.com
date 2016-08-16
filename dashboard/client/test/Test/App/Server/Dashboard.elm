module Test.Server.Dashboard exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Moment exposing (Moment)
import Task exposing (Task)
import Url exposing (Url)

import App.Error exposing (..)
import App.Http exposing (..)
import App.Model exposing (..)
import App.Server.Dashboard exposing (..)

tests : Tests
tests =
  [
    test "Server.Dashboard.fetch: fetches a dashboard full of pull requests" (
      let
        expected : Task Error (Response Dashboard)
        expected = dashboard |> Result.map Response |> Result.formatError UnexpectedResponse |> Task.fromResult

        actual : Task Error (Response Dashboard)
        actual = fetch (App.Http.stubGet "/dashboard" dashboardJson)
      in
        assert actual (equals expected)
    ),

    test "Server.Dashboard.fetch: recognises an unauthenticated response" (
      let
        expected : Task Error (Response Dashboard)
        expected = Task.succeed UnauthenticatedResponse

        actual : Task Error (Response Dashboard)
        actual = fetch (App.Http.stubGet "/dashboard" unauthenticatedJson)
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
          link = Url.parse "https://github.com/sandwiches/cheese"
        },
        number = 123,
        title = "Add support for French cheese.",
        updatedAt = Moment.parse "2016-05-04T15:44:33Z",
        link = Url.parse "https://github.com/sandwiches/cheese/pull/123"
      },
      {
        repository = {
          owner = "sandwiches",
          name = "cheese",
          link = Url.parse "https://github.com/sandwiches/cheese"
        },
        number = 121,
        title = "Discontinue pre-sliced cheese wrapped in plastic.",
        updatedAt = Moment.parse "2016-02-06T03:08:56Z",
        link = Url.parse "https://github.com/sandwiches/cheese/pull/121"
      }
    ]
  }

dashboardJson : String
dashboardJson =
  """
    {
      "state": "Authenticated",
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

unauthenticatedJson : String
unauthenticatedJson =
  """
    {
      "state": "Unauthenticated"
    }
  """

dashboardResult
    : { now: Result String Moment,
        pullRequests: List { repository : Repository, number : Int, title : String, updatedAt : Result String Moment, link : Url } }
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
