import GitHub.PullRequests
import Model exposing (..)

import Page.Dashboard
import Page.Error
import Page.Loading

import Effects exposing (Effects)
import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import Http
import Moment
import Signal exposing (Signal)
import StartApp exposing (App, start)
import Task exposing (Task)

type Action = Display Model

init : (Model, Effects Action)
init = (Loading, fetch)

fetch : Effects Action
fetch =
  let
    now = Moment.now ()
    gitHubPullRequests = GitHub.PullRequests.fetch Http.get {owner = "elm-lang", repository = "core"}
  in
    Task.map2 createDashboard now gitHubPullRequests
      |> Task.map Display
      |> (flip Task.onError) (Task.succeed << Display << Error)
      |> Effects.task

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Display newModel -> (newModel, Effects.none)

view : Signal.Address Action -> Model -> Html
view _ model =
  div [id "container", class "container-fluid"]
    <| case model of
      Loading -> Page.Loading.html
      Dashboard dashboard -> Page.Dashboard.html dashboard
      Error error -> Page.Error.html error

app : App Model
app =
  start {
    init = init,
    update = update,
    view = view,
    inputs = []
  }

main : Signal Html
main = app.html

port tasks : Signal (Task Effects.Never ())
port tasks = app.tasks
