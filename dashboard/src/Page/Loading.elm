module Page.Loading where

import Html exposing (..)
import Html.Attributes exposing (class)

html =
  div [class "loading"] [
    h1 [] [text "Loading…"]
  ]
