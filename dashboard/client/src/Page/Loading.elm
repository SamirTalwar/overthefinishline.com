module Page.Loading exposing (html)

import Html exposing (..)

html : List (Html a)
html =
  [h1 [] [text "Loading…"]]
