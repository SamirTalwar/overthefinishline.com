module App.Page.Home exposing (html)

import Html exposing (..)
import Html.Attributes exposing (action, class, href, method, type_)

html : List (Html a)
html =
  [
    h1 [] [text "Measure your work in progress."],
    p [] [text "Keep an eye on your work in progress. Focus on completing tasks, not starting new ones."],

    hr [] [],
    p [] [text "At the moment, Over The Finish Line only supports monitoring GitHub pull requests, but more will be coming soon."],

    form [method "post", action "/authentication/by/github"] [
      button [type_ "submit", class "sign-in"] [
        span [class "fa fa-github"] [],
        text "Sign in with GitHub"
      ]
    ]
  ]
