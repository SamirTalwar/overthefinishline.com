module GitHub.PullRequests (
    PullRequest,
    PullRequests,
    fetch
  ) where

import Error exposing (Error)
import Http
import Json.Decode exposing (..)
import Task exposing (Task)

type alias Repository = { owner : String, repository : String }
type alias PullRequest = { title : String }
type alias PullRequests = List PullRequest

type alias HttpGet = Decoder PullRequests -> String -> Task Http.Error PullRequests

root = "https://api.github.com"

fetch : HttpGet -> Repository -> Task Error PullRequests
fetch get { owner, repository } =
  get decoder (root ++ "/repos/" ++ owner ++ "/" ++ repository ++ "/pulls")
    |> Task.mapError (\error -> case error of
         Http.Timeout -> Error.FailureToConnect
         Http.NetworkError -> Error.FailureToConnect
         Http.UnexpectedPayload _ -> Error.UnexpectedResponse
         Http.BadResponse _ _ -> Error.UnexpectedResponse)

decoder = list (object1 PullRequest ("title" := string))
