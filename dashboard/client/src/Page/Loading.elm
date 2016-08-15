module Page.Loading exposing (html)

import Html exposing (..)

html : Maybe String -> List (Html a)
html extraText =
  case extraText of
    Nothing ->
      [h1 [] [text "Loading…"]]
    Just extra ->
      [h1 [] [text "Loading…"], h2 [] [text extra]]
