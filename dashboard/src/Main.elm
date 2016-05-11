import GitHub.PullRequests
import Model exposing (..)

import Page.Dashboard
import Page.Error
import Page.Loading

import Html exposing (Html, div)
import Html.App exposing (program)
import Html.Attributes exposing (class, id)
import Http
import Moment
import Task exposing (Task)

type Message = Display Model

init : (Model, Cmd Message)
init = (Loading, fetch)

fetch : Cmd Message
fetch =
  let
    now = Moment.now ()
    gitHubPullRequests = GitHub.PullRequests.fetch Http.get {owner = "elm-lang", repository = "core"}
  in
    Task.map2 createDashboard now gitHubPullRequests
      |> Task.perform (Display << Error) (Display)

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    Display newModel -> (newModel, Cmd.none)

view : Model -> Html Message
view model =
  div [id "container", class "container-fluid"]
    <| case model of
      Loading -> Page.Loading.html
      Dashboard dashboard -> Page.Dashboard.html dashboard
      Error error -> Page.Error.html error

main : Program Never
main =
  program {
    init = init,
    update = update,
    view = view,
    subscriptions = always Sub.none
  }
