module App.Server.Me exposing (decoder)

import Json.Decode exposing (..)
import Url exposing (Url)

import App.Model exposing (..)

decoder : Decoder Me
decoder =
  at ["user", "username"] string `andThen` \username ->
    object2 Me
      ("user" := object2 User
        (succeed username)
        ("avatarUrl" := object1 (GitHubAvatar << Url.parse) string))
      ("projects" := list
        (object3 Project
          (succeed username)
          ("name" := string)
          (succeed [])))
