module Test.App.Server.Dashboard exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Json.Decode exposing (decodeString)
import Moment exposing (Moment)
import Task exposing (Task)
import Test.Helpers.Task exposing (taskFromResult)
import Url exposing (Url)

import App.Location as Location exposing (Location)
import App.Model exposing (..)
import App.Server.Dashboard exposing (..)

tests : Tests
tests =
  [
    test "App.Server.Dashboard.decoder: decodes a dashboard full of pull requests" (
      let
        location = Location.Project "sandwiches" "cheese"

        expected : Task String Dashboard
        expected = dashboard location |> taskFromResult

        actual : Task String Dashboard
        actual = decodeString (decoder location) dashboardJson |> taskFromResult
      in
        assert actual (equals expected)
    )
  ]

dashboard : Location -> Result String Dashboard
dashboard location =
  dashboardResult location {
    now = Moment.parse "2016-06-01T08:00:00Z",
    pullRequests = [
      {
        repository = {
          owner = "sandwiches",
          name = "cheese",
          url = Url.parse "https://github.com/sandwiches/cheese"
        },
        number = 123,
        title = "Add support for French cheese.",
        status = Success,
        updatedAt = Moment.parse "2016-05-04T15:44:33Z",
        url = Url.parse "https://github.com/sandwiches/cheese/pull/123"
      },
      {
        repository = {
          owner = "sandwiches",
          name = "cheese",
          url = Url.parse "https://github.com/sandwiches/cheese"
        },
        number = 121,
        title = "Discontinue pre-sliced cheese wrapped in plastic.",
        status = NoStatus,
        updatedAt = Moment.parse "2016-02-06T03:08:56Z",
        url = Url.parse "https://github.com/sandwiches/cheese/pull/121"
      }
    ]
  }

dashboardJson : String
dashboardJson =
  """
    {
      "now": "2016-06-01T08:00:00Z",
      "pullRequests": [
        {
          "repository": {
            "owner": "sandwiches",
            "name": "cheese",
            "url": "https://github.com/sandwiches/cheese"
          },
          "number": 123,
          "title": "Add support for French cheese.",
          "status": "success",
          "updatedAt": "2016-05-04T15:44:33Z",
          "url": "https://github.com/sandwiches/cheese/pull/123"
        },
        {
          "repository": {
            "owner": "sandwiches",
            "name": "cheese",
            "url": "https://github.com/sandwiches/cheese"
          },
          "number": 121,
          "title": "Discontinue pre-sliced cheese wrapped in plastic.",
          "status": "none",
          "updatedAt": "2016-02-06T03:08:56Z",
          "url": "https://github.com/sandwiches/cheese/pull/121"
        }
      ]
    }
  """

dashboardResult
    : Location
    -> { now: Result String Moment,
        pullRequests: List {
          repository : Repository,
          number : Int,
          title : String,
          status : ItemStatus,
          updatedAt : Result String Moment,
          url : Url } }
    -> Result String Dashboard
dashboardResult location {now, pullRequests} =
  let
    pullRequestsResult = pullRequests
      |> List.map (\record ->
           case record.updatedAt of
             Ok updatedAtMoment -> Ok { record | updatedAt = updatedAtMoment }
             Err error -> Err error)
      |> sequenceResults
  in
    Result.map2 (Dashboard location) now pullRequestsResult

sequenceResults : List (Result a b) -> Result a (List b)
sequenceResults = List.foldr (Result.map2 (::)) (Ok [])
