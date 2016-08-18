module App.Model exposing (..)

import Moment exposing (Moment)
import Url exposing (Url)

import App.Error exposing (Error)
import App.Http exposing (Response)
import App.Navigation as Navigation

type Message =
    MeMessage (Response Me)
  | NavigationMessage Navigation.Message
  | NavigateTo Url
  | ErrorMessage Error

type Model =
    Loading
  | Unauthenticated
  | CatastrophicFailure Error
  | Model Me Navigation.State (Maybe Dashboard) (Maybe Error)

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
