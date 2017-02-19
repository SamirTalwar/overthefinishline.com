module App.Server.Dashboard exposing (decoder)

import Json.Decode exposing (..)
import Moment exposing (Moment)
import Url exposing (Url)

import App.Location as Location exposing (Location)
import App.Model exposing (..)

decoder : Location -> Decoder Dashboard
decoder location = map2 (Dashboard location)
  (field "now" Moment.decode)
  (field "pullRequests" <| list
    (map6 PullRequest
      (map3 Repository
        (at ["repository", "owner"] string)
        (at ["repository", "name"] string)
        (at ["repository", "url"] Url.decoder)
      )
      (field "number" int)
      (field "title" string)
      (maybe (field "status" itemStatusDecoder) |> map (Maybe.withDefault NoStatus))
      (field "updatedAt" Moment.decode)
      (field "url" Url.decoder)))

itemStatusDecoder : Decoder ItemStatus
itemStatusDecoder = string |> andThen (\status -> case status of
  "failure" -> succeed Failure
  "pending" -> succeed Pending
  "success" -> succeed Success
  other -> fail ("Invalid item status: " ++ other))
