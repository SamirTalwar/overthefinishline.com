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

type Message = UserMessage (Response User) | ErrorMessage Error

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
    UserMessage (Response user) -> (Model user DashboardLoading, Cmd.none)
    UserMessage UnauthenticatedResponse -> (Unauthenticated, Cmd.none)
    ErrorMessage error -> (Error error, Cmd.none)

view : Model -> Html Message
view model =
  case model of
    Loading -> Page.Frame.html Nothing Page.Loading.html
    Unauthenticated -> Page.Frame.html Nothing Page.Authentication.html
    Error error -> Page.Frame.html Nothing (Page.Error.html error)
    Model user DashboardLoading -> Page.Frame.html (Just user) Page.Loading.html
    Model user (Dashboard now pullRequests) -> Page.Frame.html (Just user) (Page.Dashboard.html now pullRequests)
