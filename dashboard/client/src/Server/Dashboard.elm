module Server.Dashboard exposing (fetch)

import Error exposing (Error)
import Model exposing (..)

import Http
import Json.Decode exposing (..)
import Moment exposing (Moment)
import Task exposing (Task)

type alias HttpGet = Decoder Model -> String -> Task Http.Error Model

fetch : HttpGet -> Task Error Model
fetch get =
  get decoder "/dashboard"
    |> Task.mapError (\error -> case error of
         Http.Timeout -> Error.FailureToConnect
         Http.NetworkError -> Error.FailureToConnect
         Http.UnexpectedPayload payload -> Error.UnexpectedResponse payload
         Http.BadResponse status message -> Error.UnexpectedResponse (toString status ++ " " ++ message))

decoder : Decoder Model
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
