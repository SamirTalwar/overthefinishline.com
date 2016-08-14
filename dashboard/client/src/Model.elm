module Model exposing (..)

import Moment exposing (Moment)
import Result

import Error exposing (Error)

type Model = Loading
           | Error Error
           | Unauthenticated
           | Model Username Projects Dashboard

type User = UnauthenticatedUser
          | AuthenticatedUser {
              username : String,
              projects : List Project
            }

type alias Projects = List Project
type alias Project = {
    name : String,
    link : Link
  }

type Dashboard =
    DashboardLoading
  | Dashboard {
      now : Moment,
      pullRequests : PullRequests
    }

type alias PullRequests = List PullRequest
type alias PullRequest = {
    repository : Repository,
    number : Int,
    title : String,
    updatedAt : Moment,
    link : Link
  }

type alias Repository = {
    owner : String,
    name : String,
    link : Link
  }

type alias Username = String

type alias Link = String

createDashboard : Moment -> PullRequests -> Dashboard
createDashboard now pullRequests = Dashboard { now = now, pullRequests = pullRequests }

dashboard
    : { now: Result String Moment,
        pullRequests: List { repository : Repository, number : Int, title : String, updatedAt : Result String Moment, link : Link } }
    -> Result String Dashboard
dashboard {now, pullRequests} =
  let
    pullRequestsResult = pullRequests
      |> List.map (\record ->
           case record.updatedAt of
             Ok updatedAtMoment -> Ok { record | updatedAt = updatedAtMoment }
             Err error -> Err error)
      |> sequenceResults
  in
    Result.map2 createDashboard now pullRequestsResult

pullRequests
    : List { repository : Repository, number : Int, title : String, updatedAt : Result String Moment, link : Link }
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
