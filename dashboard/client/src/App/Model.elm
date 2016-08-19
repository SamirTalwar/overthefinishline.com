module App.Model exposing (..)

import Moment exposing (Moment)
import Url exposing (Url)

import App.Error exposing (Error)
import App.Location exposing (Location)
import App.Http exposing (Response)
import App.Navigation as Navigation

type Message =
    Load Location
  | NavigateTo Location
  | MeMessage (Response Me)
  | NavigationMessage Navigation.Message
  | NewProjectMessage (List Name)
  | ErrorMessage Error

type Model =
    Loading
  | Unauthenticated
  | CatastrophicFailure Error
  | Model Me Navigation.State Page

type Page =
    LoadingPage
  | SelectAProjectPage Projects
  | NewProjectPage (List Name)
  | DashboardPage Dashboard
  | ErrorPage Error

type Me = Me User Projects

type User = User Username Avatar

type Dashboard = Dashboard Moment PullRequests

type alias Projects = List Project
type Project = Project Name Location

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
