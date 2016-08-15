module Model exposing (..)

import Moment exposing (Moment)
import Url exposing (Url)

import Error exposing (Error)

type Response a =
    UnauthenticatedResponse
  | Response a

type Model =
    Loading
  | Error Error
  | Unauthenticated
  | NoProjectSelected Me
  | ProjectDashboard Me Dashboard

type Me = Me User Projects

type User = User Username Avatar

type alias Projects = List Project
type Project = Project Name Url

type Dashboard = Dashboard Moment PullRequests

type alias PullRequests = List PullRequest
type alias PullRequest = {
    repository : Repository,
    number : Int,
    title : String,
    updatedAt : Moment,
    link : Url
  }

type alias Repository = {
    owner : String,
    name : String,
    link : Url
  }

type Avatar = GitHubAvatar Url

avatarLink : Avatar -> Url
avatarLink (GitHubAvatar url) = Url.addQuery "s" "24" url

type alias Name = String
type alias Username = String
