module Server.Me exposing (fetch)

import Error exposing (Error)
import Model exposing (..)

import Http
import HttpX
import Json.Decode exposing (..)
import Moment exposing (Moment)
import Task exposing (Task)

fetch : HttpX.Get User -> Task Error User
fetch get = get decoder "/me" |> Task.mapError HttpX.handleError

decoder : Decoder User
decoder =
  ("tag" := string) `andThen` \tag ->
    case tag of
      "AuthenticatedUser" ->
        object2 AuthenticatedUser
          ("username" := string)
          ("projects" := list
            (object2 Project
              ("name" := string)
              ("link" := string)))
      "UnauthenticatedUser" ->
        succeed UnauthenticatedUser
      _ ->
          fail (tag ++ " is not a recognized tag")
