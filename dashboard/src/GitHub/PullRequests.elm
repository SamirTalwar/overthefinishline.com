module GitHub.PullRequests (fetch) where

import Error exposing (Error)
import Model exposing (PullRequest, PullRequests, Repository)

import Date exposing (Date)
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

decoder : Decoder PullRequests
decoder =
  list
    <| object4 PullRequest
      (object2 Repository (at ["base", "repo", "owner", "login"] string) (at ["base", "repo", "name"] string))
      ("number" := int)
      ("title" := string)
      ("updated_at" := date)

date : Decoder Date
date = customDecoder string Date.fromString
