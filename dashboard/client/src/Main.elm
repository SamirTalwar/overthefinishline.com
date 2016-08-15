module Main exposing (main)

import Html exposing (Html, div)
import Html.App exposing (program)
import Http
import Task exposing (Task)

import Error exposing (..)
import Model exposing (..)
import Server.Me

import Page.Authentication
import Page.Dashboard
import Page.Error
import Page.Frame
import Page.Loading

type Message = MeMessage (Response Me) | ErrorMessage Error

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
update message _ =
  case message of
    MeMessage (Response me) -> (NoProjectSelected me, Cmd.none)
    MeMessage UnauthenticatedResponse -> (Unauthenticated, Cmd.none)
    ErrorMessage error -> (Error error, Cmd.none)

view : Model -> Html Message
view model =
  case model of
    Loading -> Page.Frame.html Nothing Page.Loading.html
    Unauthenticated -> Page.Frame.html Nothing Page.Authentication.html
    Error error -> Page.Frame.html Nothing (Page.Error.html error)
    NoProjectSelected me -> Page.Frame.html (Just me) Page.Loading.html
    ProjectDashboard me dashboard -> Page.Frame.html (Just me) (Page.Dashboard.html dashboard)
