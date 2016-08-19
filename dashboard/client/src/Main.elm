module Main exposing (main)

import Html exposing (Html, div)
import Html.App
import Navigation
import Process
import Task exposing (Task)
import Time

import App.Error exposing (..)
import App.Http exposing (Response (..))
import App.Location as Location exposing (Location)
import App.Model exposing (..)
import App.Navigation
import App.Server.Me as Me

import App.Page.Authentication
import App.Page.Dashboard
import App.Page.Error
import App.Page.Frame
import App.Page.Loading
import App.Page.Navigation
import App.Page.NewProject
import App.Page.SelectAProject

main : Program Never
main =
  Navigation.program (Navigation.makeParser Location.parser) {
    init = init,
    update = update,
    urlUpdate = urlUpdate,
    view = view,
    subscriptions = always Sub.none
  }

init : Location -> (Model, Cmd Message)
init location =
  Loading ! [
    Me.fetch App.Http.get |> Task.perform ErrorMessage MeMessage,
    load location
  ]

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case (message, model) of
    (Load Location.Home, Model me navigationState _) ->
      let (Me _ projects) = me
      in Model me navigationState (SelectAProjectPage projects) ! []
    (Load Location.NewProject, Model me navigationState _) ->
      let (Me _ projects) = me
      in Model me navigationState (NewProjectPage []) ! []
    (Load (Location.Project url), Model me navigationState _) ->
      let (Me _ projects) = me
      in Model me navigationState (ErrorPage (MissingPage "project")) ! []
    (Load (Location.Error log), Model me navigationState _) ->
      let (Me _ projects) = me
      in Model me navigationState (ErrorPage (UnknownError log)) ! []
    (Load location, Loading) ->
      Loading ! [load location]
    (Load location, Unauthenticated) ->
      Unauthenticated ! []
    (Load location, CatastrophicFailure error) ->
      CatastrophicFailure error ! []

    (NavigateTo location, model) ->
      model ! [Location.navigateTo location]

    (MeMessage UnauthenticatedResponse, _) ->
      Unauthenticated ! []
    (MeMessage (Response me), _) ->
      (Model me App.Navigation.initialState LoadingPage ! [])

    (NavigationMessage message, Model me oldNavigationState page) ->
      let (navigationState, command) = App.Navigation.update message oldNavigationState
      in Model me navigationState page ! [Cmd.map NavigationMessage command]
    (NavigationMessage _, model) ->
      model ! []

    (NewProjectMessage repositoryNames, Model me navigationState (NewProjectPage _)) ->
      Model me navigationState (NewProjectPage repositoryNames) ! []
    (NewProjectMessage repositoryNames, _) ->
      model ! []

    (ErrorMessage error, Model me navigationState _) ->
      Model me navigationState (ErrorPage error) ! []
    (ErrorMessage error, _) ->
      CatastrophicFailure error ! []

urlUpdate : Location -> Model -> (Model, Cmd Message)
urlUpdate location model = (model ! [load location])

load : Location -> Cmd Message
load location =
  Process.sleep (100 * Time.millisecond)
    |> Task.map (always location)
    |> Task.perform ErrorMessage Load

view : Model -> Html Message
view model =
  case model of
    Loading -> App.Page.Frame.html [navigationSignedOut] App.Page.Loading.html
    Unauthenticated -> App.Page.Frame.html [navigationSignedOut] App.Page.Authentication.html
    CatastrophicFailure error -> App.Page.Frame.html [navigationSignedOut] (App.Page.Error.html error)
    Model me navigationState page ->
      App.Page.Frame.html [navigationSignedIn me navigationState] <| case page of
        LoadingPage -> App.Page.Loading.html
        SelectAProjectPage projects -> App.Page.SelectAProject.html projects
        NewProjectPage repositoryNames -> App.Page.NewProject.html repositoryNames
        DashboardPage dashboard -> App.Page.Dashboard.html dashboard
        ErrorPage error -> App.Page.Error.html error

navigationSignedIn : Me -> App.Navigation.State -> Html Message
navigationSignedIn me state = Html.App.map NavigationMessage (App.Page.Navigation.signedIn me state)

navigationSignedOut : Html Message
navigationSignedOut = Html.App.map NavigationMessage App.Page.Navigation.signedOut
