module Server.Me exposing (fetch)

import Error exposing (Error)
import Model exposing (..)

import Erl exposing (Url)
import HttpX
import Json.Decode exposing (..)
import Task exposing (Task)

fetch : HttpX.Get (Response Me) -> Task Error (Response Me)
fetch get = get decoder "/me" |> Task.mapError HttpX.handleError

decoder : Decoder (Response Me)
decoder =
  ("state" := string) `andThen` \state ->
    case state of
      "Authenticated" ->
        object1 Response <| object2 Me
          ("user" := object2 User
            ("username" := string)
            ("avatarUrl" := object1 (GitHubAvatar << Erl.parse) string))
          ("projects" := list
            (object2 Project
              ("name" := string)
              ("link" := string)))
      "Unauthenticated" ->
        succeed UnauthenticatedResponse
      _ ->
          fail (state ++ " is not a recognized state")