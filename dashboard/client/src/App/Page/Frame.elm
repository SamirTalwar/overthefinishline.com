module App.Page.Frame exposing (html)

import Html exposing (..)
import Html.Attributes exposing (..)

html : List (Html a) -> List (Html a) -> Html a
html topContent mainContent =
  div [id "root"] [
    div [class "top"] topContent,
    div [id "container", class "container-fluid"] mainContent
  ]
