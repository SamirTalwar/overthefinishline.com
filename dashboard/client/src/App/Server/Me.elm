module App.Server.Me exposing (decoder)

import Json.Decode exposing (..)
import Url exposing (Url)

import App.Model exposing (..)

decoder : Decoder Me
decoder =
  at ["user", "username"] string |> andThen (\username ->
    map2 Me
      (field "user" <| map2 User
        (succeed username)
        (field "avatarUrl" <| map (GitHubAvatar << Url.parse) string))
      (field "projects" <| list
        (map3 Project
          (succeed username)
          (field "name" string)
          (succeed []))))
