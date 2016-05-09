import GitHub.PullRequests exposing (PullRequest)

import Effects
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import List
import StartApp exposing (start)
import Task exposing (Task)

type Model = Loading | Error String | Dashboard (List PullRequest)

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

update result _ = (result, Effects.none)

view _ model =
  case model of
    Loading ->
      div [class "loading"] [
        h1 [] [text "Loading…"]
      ]
    Dashboard prs -> div [] (List.map (\pr -> p [] [text pr.title]) prs)
    Error error -> p [] [text error]

app =
  start {
    init = init,
    update = update,
    view = view,
    inputs = []
  }

main = app.html

port tasks : Signal (Task Effects.Never ())
port tasks = app.tasks
