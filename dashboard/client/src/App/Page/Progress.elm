module App.Page.Progress exposing (report)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import App.Page.Html exposing (role)

import App.Model exposing (Progress (..))
import App.Page.Error as ErrorPage

report : Progress -> Html a
report progress =
  div [class "progress-report"] <| case progress of
    Finished -> []
    Loading ->
      [div [class "progress"] [
        div
          [class "progress-bar progress-bar-striped progress-bar-animated",
           role "progressbar",
           style [("width", "100%"), ("height", "0.5rem")]]
          []]]
    Error error ->
      [div
        [class "container container-fluid",
         style [("margin-top", "1rem")]]
        (ErrorPage.html error)]
