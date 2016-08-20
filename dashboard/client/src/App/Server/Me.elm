module App.Server.Me exposing (endpoint)

import Json.Decode exposing (..)
import Url exposing (Url)

import App.Location as Location exposing (Location)
import App.Model exposing (..)

endpoint : (Location, Decoder Me)
endpoint = (Location.Me, decoder)

decoder : Decoder Me
decoder = object2 Me
  ("user" := object2 User
    ("username" := string)
    ("avatarUrl" := object1 (GitHubAvatar << Url.parse) string))
  ("projects" := list
    (object2 Project
      ("name" := string)
      ("url" := object1 (Location.Project << Url.parse) string)))
