module Page.SelectAProject exposing (html)

import Html exposing (..)

html : List (Html a)
html =
  [h1 [] [text "Please select a project."]]
