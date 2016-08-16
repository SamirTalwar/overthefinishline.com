module App.Server.Dashboard exposing (fetch)

import App.Error exposing (Error)
import App.Model exposing (..)

import App.Http exposing (..)
import Json.Decode exposing (..)
import Moment exposing (Moment)
import Task exposing (Task)
import Url

fetch : Get Dashboard -> Task Error (Response Dashboard)
fetch get = get decoder "/dashboard"

decoder : Decoder Dashboard
decoder = object2 Dashboard
  ("now" := Moment.decode)
  ("pullRequests" := list
    (object5 PullRequest
      (object3 Repository
        (at ["repository", "owner"] string)
        (at ["repository", "name"] string)
        (at ["repository", "link"] Url.decoder)
      )
      ("number" := int)
      ("title" := string)
      ("updatedAt" := Moment.decode)
      ("link" := Url.decoder)))
