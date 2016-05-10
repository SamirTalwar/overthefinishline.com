module Model where

import Error exposing (Error)

type Model = Loading | Error Error | Dashboard { pullRequests : PullRequests }

type alias PullRequests = List PullRequest
type alias PullRequest = { title : String }
type alias Repository = { owner : String, repository : String }
