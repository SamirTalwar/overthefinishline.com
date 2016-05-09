module Page.Loading where

import Html exposing (..)
import Html.Attributes exposing (class)

html =
  div [class "one-thing-only"] [
    h1 [] [text "Loading…"]
  ]
