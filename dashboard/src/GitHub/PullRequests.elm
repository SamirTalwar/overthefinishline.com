module GitHub.PullRequests (
    PullRequest (..),
    fetch
  ) where

import Json.Decode exposing (..)
import Task

type PullRequest = PullRequest { title: String }

root = "https://api.github.com"

fetch get { owner, repository } =
  get decoder (root ++ "/repos/" ++ owner ++ "/" ++ repository ++ "/pulls")

decoder = list (object1 createPullRequest ("title" := string))

createPullRequest title = PullRequest { title = title }
