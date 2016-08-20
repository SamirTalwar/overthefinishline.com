module App.Server.Dashboard exposing (fetch)

import App.Error exposing (Error)
import App.Location as Location exposing (Location)
import App.Model exposing (..)

import App.Http exposing (..)
import Json.Decode exposing (..)
import Moment exposing (Moment)
import Task exposing (Task)
import Url exposing (Url)

fetch : Get Dashboard -> Location -> Task Error (Response Dashboard)
fetch get location =
  get (decoder location) (Url.toString (Location.url location))

decoder : Location -> Decoder Dashboard
decoder location = object2 (Dashboard location)
  ("now" := Moment.decode)
  ("pullRequests" := list
    (object5 PullRequest
      (object3 Repository
        (at ["repository", "owner"] string)
        (at ["repository", "name"] string)
        (at ["repository", "url"] Url.decoder)
      )
      ("number" := int)
      ("title" := string)
      ("updatedAt" := Moment.decode)
      ("url" := Url.decoder)))
