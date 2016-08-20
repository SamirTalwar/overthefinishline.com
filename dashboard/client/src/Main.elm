module Main exposing (main)

import Html exposing (Html, div)
import Html.App
import Navigation
import Process
import Task exposing (Task)
import Time exposing (every, minute)

import App.Error exposing (..)
import App.Http exposing (Response (..))
import App.Location as Location exposing (Location)
import App.Model exposing (..)
import App.Navigation
import App.Server.Dashboard
import App.Server.Me

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
    subscriptions = subscriptions
  }

init : Location -> (Model, Cmd Message)
init location =
  Loading ! [fetchMe, delay (Load location)]

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case (message, model) of
    (Load location, Loading) ->
      Loading ! [delay (Load location)]
    (Load location, Unauthenticated) ->
      Unauthenticated ! []
    (Load location, CatastrophicFailure error) ->
      CatastrophicFailure error ! []
    (Load location, (Model me navigationState page)) ->
      Model me navigationState LoadingPage ! [fetch me location]

    (NavigateTo location, model) ->
      model ! [Location.navigateTo location]

    (Render page, Model me navigationState _) ->
      Model me navigationState page ! []
    (Render page, model) ->
      model ! [delay (Render page)]

    (MeMessage UnauthenticatedResponse, _) ->
      Unauthenticated ! []
    (MeMessage (Response me), _) ->
      Model me App.Navigation.initialState LoadingPage ! []

    (DashboardMessage UnauthenticatedResponse, _) ->
      Unauthenticated ! []
    (DashboardMessage (Response dashboard), Model me navigationState _) ->
      Model me navigationState (DashboardPage dashboard) ! []
    (DashboardMessage _, model) ->
      model ! []

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
urlUpdate location model = (model ! [send Load (Task.succeed location)])

delay : Message -> Cmd Message
delay message =
  Process.sleep (100 * Time.millisecond)
    |> Task.map (always message)
    |> send identity

fetch : Me -> Location -> Cmd Message
fetch (Me _ projects) location =
  case location of
    Location.Me -> fetchMe
    Location.Home -> Task.succeed (SelectAProjectPage projects) |> send Render
    Location.NewProject -> Task.succeed (NewProjectPage []) |> send Render
    Location.Project url -> App.Http.get (App.Server.Dashboard.endpoint (Location.Project url)) |> send DashboardMessage
    Location.Error error -> Task.succeed (ErrorPage (UnknownError error)) |> send Render

fetchMe : Cmd Message
fetchMe = App.Http.get App.Server.Me.endpoint |> send MeMessage

send : (a -> Message) -> Task Error a -> Cmd Message
send = Task.perform ErrorMessage

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

subscriptions : Model -> Sub Message
subscriptions model =
  case model of
    Model me navigationState (DashboardPage (Dashboard location _ _)) ->
      every minute (always (Load location))
    _ -> Sub.none

navigationSignedIn : Me -> App.Navigation.State -> Html Message
navigationSignedIn me state = Html.App.map NavigationMessage (App.Page.Navigation.signedIn me state)

navigationSignedOut : Html Message
navigationSignedOut = Html.App.map NavigationMessage App.Page.Navigation.signedOut
