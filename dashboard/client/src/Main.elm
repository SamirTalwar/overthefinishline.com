module Main exposing (main)

import Html exposing (Html, div)
import Html.App
import Navigation
import Task exposing (Task)
import Url

import App.Http exposing (Response (..))
import App.Location exposing (Location)
import App.Model exposing (..)
import App.Navigation
import App.Server.Me as Me

import App.Page.Authentication
import App.Page.Dashboard
import App.Page.Error
import App.Page.Frame
import App.Page.Loading
import App.Page.Navigation
import App.Page.SelectAProject

main : Program Never
main =
  Navigation.program (Navigation.makeParser App.Location.parser) {
    init = init,
    update = update,
    urlUpdate = urlUpdate,
    view = view,
    subscriptions = always Sub.none
  }

init : Location -> (Model, Cmd Message)
init = always (Loading, Me.fetch App.Http.get |> Task.perform ErrorMessage MeMessage)

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case (message, model) of
    (MeMessage UnauthenticatedResponse, _) ->
      (Unauthenticated, Cmd.none)
    (MeMessage (Response me), _) ->
      let (Me _ projects) = me
      in (Model me App.Navigation.initialState (SelectAProjectPage projects), Cmd.none)
    (NavigationMessage message, Model me navigationState dashboard) ->
      let (state, command) = App.Navigation.update message navigationState
      in (Model me state dashboard, Cmd.map NavigationMessage command)
    (NavigationMessage _, model) ->
      (model, Cmd.none)
    (ErrorMessage error, Model me navigationState _) ->
      (Model me navigationState (ErrorPage error), Cmd.none)
    (ErrorMessage error, _) ->
      (CatastrophicFailure error, Cmd.none)
    (NavigateTo url, model) ->
      (model, Navigation.newUrl (Url.toString url))

urlUpdate : Location -> Model -> (Model, Cmd Message)
urlUpdate _ model = (model, Cmd.none)

view : Model -> Html Message
view model =
  case model of
    Loading -> App.Page.Frame.html [navigationSignedOut] App.Page.Loading.html
    Unauthenticated -> App.Page.Frame.html [navigationSignedOut] App.Page.Authentication.html
    CatastrophicFailure error -> App.Page.Frame.html [navigationSignedOut] (App.Page.Error.html error)
    Model me navigationState page ->
      App.Page.Frame.html [navigationSignedIn me navigationState] <| case page of
        SelectAProjectPage projects -> App.Page.SelectAProject.html projects
        DashboardPage dashboard -> App.Page.Dashboard.html dashboard
        ErrorPage error -> App.Page.Error.html error

navigationSignedIn : Me -> App.Navigation.State -> Html Message
navigationSignedIn me state = Html.App.map NavigationMessage (App.Page.Navigation.signedIn me state)

navigationSignedOut : Html Message
navigationSignedOut = Html.App.map NavigationMessage App.Page.Navigation.signedOut
