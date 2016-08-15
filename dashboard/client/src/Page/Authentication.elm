module Page.Authentication exposing (html)

import Html exposing (..)
import Html.Attributes exposing (class, href)

html : List (Html a)
html =
  [
    a [class "sign-in", href "/authentication/by/github"] [
      span [class "fa fa-github"] [],
      text "Sign in with GitHub"
    ]
  ]
