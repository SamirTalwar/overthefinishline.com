module App.Server.Dashboard exposing (decoder)

import Json.Decode exposing (..)
import Moment exposing (Moment)
import Url exposing (Url)

import App.Location as Location exposing (Location)
import App.Model exposing (..)

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
