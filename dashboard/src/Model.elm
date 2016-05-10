module Model where

import Date exposing (Date)
import Error exposing (Error)

type Model = Loading
           | Error Error
           | Dashboard {
             pullRequests : PullRequests
           }

type alias PullRequests = List PullRequest
type alias PullRequest = { repository : Repository, number : Int, title : String, updatedAt : Date }
type alias Repository = { owner : String, repository : String }

pullRequests
    : List { repository : Repository, number : Int, title : String, updatedAt : Result String Date }
    -> Result String PullRequests
pullRequests records =
 records
   |> List.map (\record ->
        case record.updatedAt of
          Ok date -> Ok { record | updatedAt = date }
          Err error -> Err error)
   |> sequenceResults

sequenceResults : List (Result a b) -> Result a (List b)
sequenceResults = List.foldr (Result.map2 (::)) (Ok [])
