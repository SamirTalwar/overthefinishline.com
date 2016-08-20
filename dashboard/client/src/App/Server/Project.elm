module App.Server.Project exposing (decoder)

import Json.Decode exposing (..)
import Url exposing (Url)

import App.Model exposing (..)

decoder : Decoder Project
decoder = object3 Project
  (at ["user", "username"] string)
  ("name" := string)
  ("repositories" := list repository)

repository : Decoder Repository
repository =
  "owner" := string `andThen` \owner ->
  "name" := string `andThen` \name ->
  "url" := Url.decoder `andThen` \url ->
    succeed { owner = owner, name = name, url = url }
