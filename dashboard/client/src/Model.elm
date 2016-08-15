module Model exposing (..)

import Erl exposing (Url)
import Moment exposing (Moment)

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
type Project = Project Name Link

type Dashboard = Dashboard Moment PullRequests

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

type Avatar = GitHubAvatar Url

avatarLink : Avatar -> Link
avatarLink (GitHubAvatar url) = Erl.addQuery "s" "32" url |> Erl.toString

type alias Name = String
type alias Username = String
type alias Link = String
