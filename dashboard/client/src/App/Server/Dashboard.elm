module App.Server.Dashboard exposing (endpoint)

import App.Location as Location exposing (Location)
import App.Model exposing (..)

import Json.Decode exposing (..)
import Moment exposing (Moment)
import Url exposing (Url)

endpoint : Location -> (Location, Decoder Dashboard)
endpoint location = (location, decoder location)

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
