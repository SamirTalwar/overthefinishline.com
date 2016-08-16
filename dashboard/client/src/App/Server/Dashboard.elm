module App.Server.Dashboard exposing (fetch)

import App.Error exposing (Error)
import App.Model exposing (..)

import App.Http exposing (Get, handleError)
import Json.Decode exposing (..)
import Moment exposing (Moment)
import Task exposing (Task)
import Url

fetch : Get (Response Dashboard) -> Task Error (Response Dashboard)
fetch get = get decoder "/dashboard" |> Task.mapError handleError

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
                (at ["repository", "link"] Url.decoder)
              )
              ("number" := int)
              ("title" := string)
              ("updatedAt" := Moment.decode)
              ("link" := Url.decoder)))
      "Unauthenticated" ->
        succeed UnauthenticatedResponse
      _ ->
          fail (state ++ " is not a recognized state")
