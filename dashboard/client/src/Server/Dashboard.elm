module Server.Dashboard exposing (fetch)

import Error exposing (Error)
import Model exposing (..)

import HttpX
import Json.Decode exposing (..)
import Moment exposing (Moment)
import Task exposing (Task)

fetch : HttpX.Get (Response Dashboard) -> Task Error (Response Dashboard)
fetch get = get decoder "/dashboard" |> Task.mapError HttpX.handleError

decoder : Decoder (Response Dashboard)
decoder =
  ("state" := string) `andThen` \state ->
    case state of
      "Authenticated" ->
        object1 Response <| object2 Dashboard
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
      "Unauthenticated" ->
        succeed UnauthenticatedResponse
      _ ->
          fail (state ++ " is not a recognized state")
