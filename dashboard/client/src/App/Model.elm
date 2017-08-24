module App.Model exposing (..)

import Moment exposing (Moment)
import Url exposing (Url)

import App.Error exposing (Error)
import App.Location exposing (Location)
import App.Navigation as Navigation

type Message =
    Load Location
  | NavigateTo Location
  | Render Page
  | MeMessage (Response Me)
  | NavigationMessage Navigation.Message
  | UpdateRepositoryNames (List Name)
  | EditProjectMessage (Response Project)
  | DashboardMessage (Response Dashboard)
  | ErrorMessage Error

type Response a =
    UnauthenticatedResponse
  | Response Failures a

type alias Failures = List Failure
type Failure =
    RequestFailure Url String

type Model =
    Unauthenticated
  | FirstLoad
  | Model Me Navigation.State Progress Page

type Progress =
    Finished
  | Loading
  | Error Error

type Page =
    LoadingPage
  | SelectAProjectPage Projects
  | NewProjectPage (List Name)
  | EditProjectPage Project
  | DashboardPage Failures Dashboard
  | ErrorPage Error

type Me = Me User Projects

type User = User Username Avatar

type Dashboard = Dashboard Location Moment PullRequests

type alias Projects = List Project
type Project = Project Username ProjectName (List RepositoryName)

type alias PullRequests = List PullRequest
type alias PullRequest = {
    repository : Repository,
    number : Int,
    title : String,
    status : ItemStatus,
    updatedAt : Moment,
    url : Url
  }

type alias Repositories = List Repository
type alias Repository = {
    owner : String,
    name : String,
    url : Url
  }

type Avatar = GitHubAvatar Url

avatarSize : Int
avatarSize = 24

avatarLink : Avatar -> Url
avatarLink (GitHubAvatar url) = Url.addQuery "s" (toString avatarSize) url

type alias Name = String
type alias Username = String
type alias ProjectName = String
type alias RepositoryName = String

type ItemStatus =
    NoStatus
  | Failure
  | Pending
  | Success
