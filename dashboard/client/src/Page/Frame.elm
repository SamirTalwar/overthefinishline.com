module Page.Frame exposing (html)

import Html exposing (..)
import Html.Attributes exposing (..)

import Model exposing (..)

html : Maybe Me -> List (Html a) -> Html a
html me mainContent =
  div [id "root"] [
    navigation me,
    div [id "container", class "container-fluid"] mainContent
  ]

navigation : Maybe Me -> Html a
navigation me =
  nav [] [
    a [class "navbar-brand", href "/"] [text "Over The Finish Line"],
    ul [] <| case me of
    Nothing -> []
    Just (Me (User username avatar) _) -> [
      li [] [
        span [class "avatar"] [img [src (avatarLink avatar), alt ""] []],
        span [class "username"] [text username]
      ]
    ]
  ]
