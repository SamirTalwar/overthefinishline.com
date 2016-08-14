module Server.Dashboard exposing (fetch)

import Error exposing (Error)
import Model exposing (..)

import Http
import HttpX
import Json.Decode exposing (..)
import Moment exposing (Moment)
import Task exposing (Task)

fetch : HttpX.Get Dashboard -> Task Error Dashboard
fetch get = get decoder "/dashboard" |> Task.mapError HttpX.handleError

decoder : Decoder Dashboard
decoder =
  object2 createDashboard
    ("now" := Moment.decode)
    ("pullRequests" := list
      (object5 PullRequest
        (object3 Repository
          (at ["repository", "owner"] string)
          (at ["repository", "name"] string)
          (at ["repository", "link"] string)
        )
        ("number" := int)
        ("title" := string)
        ("updatedAt" := Moment.decode)
        ("link" := string)))
