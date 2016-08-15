module Server.Me exposing (fetch)

import Error exposing (Error)
import Model exposing (..)

import HttpX
import Json.Decode exposing (..)
import Task exposing (Task)

fetch : HttpX.Get (Response User) -> Task Error (Response User)
fetch get = get decoder "/me" |> Task.mapError HttpX.handleError

decoder : Decoder (Response User)
decoder =
  ("state" := string) `andThen` \state ->
    case state of
      "Authenticated" ->
        object1 Response <| object2 User
          ("username" := string)
          ("projects" := list
            (object2 Project
              ("name" := string)
              ("link" := string)))
      "Unauthenticated" ->
        succeed UnauthenticatedResponse
      _ ->
          fail (state ++ " is not a recognized state")
