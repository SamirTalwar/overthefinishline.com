import Model exposing (..)
import Server.Dashboard

import Page.Authentication
import Page.Dashboard
import Page.Error
import Page.Frame
import Page.Loading

import Html exposing (Html, div)
import Html.App exposing (program)
import Http
import Task exposing (Task)

type alias Message = Model

init : (Model, Cmd Message)
init = (Loading, fetch)

fetch : Cmd Message
fetch =
  Server.Dashboard.fetch Http.get |> Task.perform Error identity

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    newModel -> (newModel, Cmd.none)

view : Model -> Html Message
view model =
  Page.Frame.html
    <| case model of
      Loading -> Page.Loading.html
      Error error -> Page.Error.html error
      Unauthenticated -> Page.Authentication.html
      Dashboard dashboard -> Page.Dashboard.html dashboard

main : Program Never
main =
  program {
    init = init,
    update = update,
    view = view,
    subscriptions = always Sub.none
  }
