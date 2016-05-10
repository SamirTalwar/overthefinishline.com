import GitHub.PullRequests
import Model exposing (..)

import Page.Dashboard
import Page.Error
import Page.Loading

import Effects
import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import Http
import Moment
import StartApp exposing (App, start)
import Task exposing (Task)

init : (Model, Effects.Effects Model)
init = (Loading, fetch)

fetch : Effects.Effects Model
fetch =
  let
    now = Moment.now ()
    gitHubPullRequests = GitHub.PullRequests.fetch Http.get {owner = "elm-lang", repository = "core"}
  in
    Task.map2 createDashboard now gitHubPullRequests
      |> (flip Task.onError) (Task.succeed << Error)
      |> Effects.task

update : Model -> action -> (Model, Effects.Effects Model)
update result _ = (result, Effects.none)

view : Signal.Address Model -> Model -> Html
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
