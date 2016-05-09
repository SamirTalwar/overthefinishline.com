module GitHub.PullRequests (
    PullRequest,
    fetch
  ) where

import Json.Decode exposing (..)

type alias PullRequest = { title: String }

root = "https://api.github.com"

fetch get { owner, repository } =
  get decoder (root ++ "/repos/" ++ owner ++ "/" ++ repository ++ "/pulls")

decoder = list (object1 PullRequest ("title" := string))
