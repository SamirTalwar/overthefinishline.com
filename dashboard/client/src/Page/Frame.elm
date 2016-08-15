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
  nav [class "navbar navbar-dark bg-inverse"] [
    a [class "navbar-brand", href "/"] [text "Over The Finish Line"],
    ul [class "nav navbar-nav"] <| case me of
      Nothing -> []
      Just (Me (User username avatar) projects) -> [
        li [class "nav-item"] [
          a [class "nav-link", href "#"] [
            span [class "avatar"] [img [src (avatarLink avatar), alt ""] []],
            span [class "username"] [text username]
          ]
        ]
      ]
  ]
