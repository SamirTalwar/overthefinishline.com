module Model exposing (..)

import Moment exposing (Moment)

import Error exposing (Error)

type Response a =
    UnauthenticatedResponse
  | Response a

type Model =
    Loading
  | Error Error
  | Unauthenticated
  | Model User Dashboard

type User = User Username Projects

type alias Projects = List Project
type Project = Project Name Link

type Dashboard =
    DashboardLoading
  | Dashboard Moment PullRequests

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

type alias Name = String
type alias Username = String
type alias Link = String
