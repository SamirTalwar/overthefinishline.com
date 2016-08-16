module App.Server.Me exposing (fetch)

import App.Error exposing (Error)
import App.Model exposing (..)

import Json.Decode exposing (..)
import App.Http exposing (Get, handleError)
import Task exposing (Task)
import Url exposing (Url)

fetch : Get (Response Me) -> Task Error (Response Me)
fetch get = get decoder "/me" |> Task.mapError handleError

decoder : Decoder (Response Me)
decoder =
  ("state" := string) `andThen` \state ->
    case state of
      "Authenticated" ->
        object1 Response <| object2 Me
          ("user" := object2 User
            ("username" := string)
            ("avatarUrl" := object1 (GitHubAvatar << Url.parse) string))
          ("projects" := list
            (object2 Project
              ("name" := string)
              ("url" := Url.decoder)))
      "Unauthenticated" ->
        succeed UnauthenticatedResponse
      _ ->
          fail (state ++ " is not a recognized state")
