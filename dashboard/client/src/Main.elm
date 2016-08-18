module Main exposing (main)

import Html exposing (Html, div)
import Html.App
import Navigation exposing (program)
import Task exposing (Task)

import App.Http exposing (Response (..))
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

type alias Location = ()

main : Program Never
main =
  program urlParser {
    init = init,
    update = update,
    urlUpdate = urlUpdate,
    view = view,
    subscriptions = always Sub.none
  }

urlParser : Navigation.Parser Location
urlParser = Navigation.makeParser (always ())

init : Location -> (Model, Cmd Message)
init = always (Loading, Me.fetch App.Http.get |> Task.perform ErrorMessage MeMessage)

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case (message, model) of
    (MeMessage UnauthenticatedResponse, _) ->
      (Unauthenticated, Cmd.none)
    (MeMessage (Response me), _) ->
      (Model me App.Navigation.initialState Nothing Nothing, Cmd.none)
    (NavigationMessage message, Model me _ dashboard error) ->
      (Model me (App.Navigation.state message) dashboard error, Cmd.none)
    (NavigationMessage _, model) ->
      (model, Cmd.none)
    (ErrorMessage error, Model me navigationState dashboard _) ->
      (Model me navigationState dashboard (Just error), Cmd.none)
    (ErrorMessage error, _) ->
      (CatastrophicFailure error, Cmd.none)

urlUpdate : Location -> Model -> (Model, Cmd Message)
urlUpdate _ model = (model, Cmd.none)

view : Model -> Html Message
view model =
  case model of
    Loading -> App.Page.Frame.html [] App.Page.Loading.html
    Unauthenticated -> App.Page.Frame.html [] App.Page.Authentication.html
    CatastrophicFailure error -> App.Page.Frame.html [] (App.Page.Error.html error)
    Model me navigationState _ (Just error) ->
      App.Page.Frame.html [navigation me navigationState] (App.Page.Error.html error)
    Model me navigationState Nothing Nothing ->
      let (Me _ projects) = me
      in App.Page.Frame.html [navigation me navigationState] (App.Page.SelectAProject.html projects)
    Model me navigationState (Just dashboard) Nothing ->
      App.Page.Frame.html [navigation me navigationState] (App.Page.Dashboard.html dashboard)

navigation : Me -> App.Navigation.State -> Html Message
navigation me state = Html.App.map NavigationMessage (App.Page.Navigation.html me state)
