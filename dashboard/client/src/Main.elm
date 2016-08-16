module Main exposing (main)

import Html exposing (Html, div)
import Html.App exposing (program)
import Http
import Task exposing (Task)

import Model exposing (..)
import Navigation
import Server.Me

import Page.Authentication
import Page.Dashboard
import Page.Error
import Page.Frame
import Page.Loading
import Page.Navigation
import Page.SelectAProject

main : Program Never
main =
  program {
    init = init,
    update = update,
    view = view,
    subscriptions = always Sub.none
  }

init : (Model, Cmd Message)
init = (Loading, Server.Me.fetch Http.get |> Task.perform ErrorMessage MeMessage)

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case (message, model) of
    (MeMessage UnauthenticatedResponse, _) ->
      (Unauthenticated, Cmd.none)
    (MeMessage (Response me), _) ->
      (Model me Navigation.initialState Nothing Nothing, Cmd.none)
    (NavigationMessage message, Model me _ dashboard error) ->
      (Model me (Navigation.state message) dashboard error, Cmd.none)
    (ErrorMessage error, Model me navigationState dashboard _) ->
      (Model me navigationState dashboard (Just error), Cmd.none)
    (_, model) ->
      (model, Cmd.none)

view : Model -> Html Message
view model =
  case model of
    Loading -> Page.Frame.html [] Page.Loading.html
    Unauthenticated -> Page.Frame.html [] Page.Authentication.html
    Model me navigationState _ (Just error) ->
      Page.Frame.html [navigation me navigationState] (Page.Error.html error)
    Model me navigationState Nothing Nothing ->
      let (Me _ projects) = me
      in Page.Frame.html [navigation me navigationState] (Page.SelectAProject.html projects)
    Model me navigationState (Just dashboard) Nothing ->
      Page.Frame.html [navigation me navigationState] (Page.Dashboard.html dashboard)

navigation : Me -> Navigation.State -> Html Message
navigation me state = Html.App.map NavigationMessage (Page.Navigation.html me state)
