module App.Server.Project exposing (decoder)

import Json.Decode exposing (..)

import App.Model exposing (..)

decoder : Decoder Project
decoder = object3 Project
  (at ["user", "username"] string)
  (at ["project", "name"] string)
  (at ["project", "repositories"] (list string))
