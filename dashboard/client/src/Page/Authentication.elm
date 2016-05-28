module Page.Authentication exposing (html)

import Html exposing (..)
import Html.Attributes exposing (class, href)

html =
  [
    a [class "sign-in", href "/authenticate"] [
      span [class "fa fa-github"] [],
      text "Sign in with GitHub"
    ]
  ]
