import Error exposing (..)
import Model exposing (..)
import Server.Me

import Page.Authentication
import Page.Dashboard
import Page.Error
import Page.Frame
import Page.Loading

import Html exposing (Html, div)
import Html.App exposing (program)
import Http
import Task exposing (Task)

type Message = UserMessage User | ErrorMessage Error

main : Program Never
main =
  program {
    init = init,
    update = update,
    view = view,
    subscriptions = always Sub.none
  }

init : (Model, Cmd Message)
init = (Loading, Server.Me.fetch Http.get |> Task.perform ErrorMessage UserMessage)

update : Message -> Model -> (Model, Cmd Message)
update message _ =
  case message of
    UserMessage (AuthenticatedUser username projects) -> (Model username projects DashboardLoading, Cmd.none)
    UserMessage UnauthenticatedUser -> (Unauthenticated, Cmd.none)
    ErrorMessage error -> (Error error, Cmd.none)

view : Model -> Html Message
view model =
  Page.Frame.html
    <| case model of
      Loading -> Page.Loading.html Nothing
      Unauthenticated -> Page.Authentication.html
      Error error -> Page.Error.html error
      Model username _ DashboardLoading -> Page.Loading.html (Just username)
      Model username _ (Dashboard now pullRequests) -> Page.Dashboard.html now pullRequests
