module GitHub.PullRequests (fetch) where

import Model exposing (PullRequest, PullRequests, Repository)

import Error exposing (Error)
import Http
import Json.Decode exposing (..)
import Task exposing (Task)

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
