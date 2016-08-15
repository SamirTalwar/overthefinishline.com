module Page.Frame exposing (html)

import Html exposing (..)
import Html.Attributes exposing (class, id)

html : List (Html a) -> Html a
html =
  div [id "container", class "container-fluid"]
