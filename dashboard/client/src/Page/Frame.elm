module Page.Frame exposing (html)

import Html exposing (..)
import Html.Attributes exposing (class, id)

import Model exposing (User)

html : Maybe User -> List (Html a) -> Html a
html _ =
  div [id "container", class "container-fluid"]
