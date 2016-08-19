module App.Server.Me exposing (fetch)

import App.Error exposing (Error)
import App.Model exposing (..)

import Json.Decode exposing (..)
import App.Http exposing (..)
import App.Location as Location
import Task exposing (Task)
import Url exposing (Url)

fetch : Get Me -> Task Error (Response Me)
fetch get = get decoder "/me"

decoder : Decoder Me
decoder = object2 Me
  ("user" := object2 User
    ("username" := string)
    ("avatarUrl" := object1 (GitHubAvatar << Url.parse) string))
  ("projects" := list
    (object2 Project
      ("name" := string)
      ("url" := object1 Location.Project string)))
