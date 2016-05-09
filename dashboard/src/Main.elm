import GitHub.PullRequests exposing (PullRequest (..))
import Styles

import Effects
import Json.Decode
import Html exposing (..)
import Html.CssHelpers
import Http
import List
import Result
import StartApp exposing (start)
import Task

type alias Error = String
type alias Model = Result Error (List PullRequest)

init = (Err "Fetching…", fetch)

fetch : Effects.Effects Model
fetch =
  GitHub.PullRequests.fetch Http.get {owner = "elm-lang", repository = "core"}
    |> Task.toResult
    |> Task.map (Result.formatError toString)
    |> Effects.task

update result _ = (result, Effects.none)

{ id, class, classList } = Html.CssHelpers.withNamespace Styles.namespace

view _ model =
  case model of
    Ok prs -> div [class [Styles.PullRequests]] (List.map (\(PullRequest pr) -> p [] [text pr.title]) prs)
    Err error -> p [] [text error]

app =
  start {
    init = init,
    update = update,
    view = view,
    inputs = []
  }

main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks
