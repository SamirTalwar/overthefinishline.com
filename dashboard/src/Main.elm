import GitHub.PullRequests exposing (PullRequest)

import Page.Dashboard
import Page.Error
import Page.Loading

import Effects
import Html exposing (Html)
import Http
import StartApp exposing (App, start)
import Task exposing (Task)

type Model = Loading | Error String | Dashboard (List PullRequest)

init : (Model, Effects.Effects Model)
init = (Loading, fetch)

fetch : Effects.Effects Model
fetch =
  GitHub.PullRequests.fetch Http.get {owner = "elm-lang", repository = "core"}
    |> Task.toResult
    |> Task.map (\result -> case result of
        Err error -> Error (toString error)
        Ok pullRequests -> Dashboard pullRequests
       )
    |> Effects.task

update : Model -> action -> (Model, Effects.Effects Model)
update result _ = (result, Effects.none)

view : Signal.Address Model -> Model -> Html
view _ model =
  case model of
    Loading -> Page.Loading.html
    Dashboard prs -> Page.Dashboard.html prs
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
