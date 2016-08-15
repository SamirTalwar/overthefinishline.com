module Page.Navigation exposing (html)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Attributes exposing (..)
import Url

import Model exposing (..)
import Navigation exposing (..)

html : Me -> State -> Html Navigation.Message
html (Me (User username avatar) projects) (ProjectsShown projectsShown) =
  nav [class "navbar navbar-dark bg-inverse"] [
    a [class "navbar-brand", href "/"] [text "Over The Finish Line"],
    ul [class "nav navbar-nav"]  [
      li [class "nav-item"] [
        a [class "nav-link", href "#"] [
          span [class "avatar"] [img [src (avatarLink avatar |> Url.toString), alt ""] []],
          span [class "username"] [text username]
        ]
      ],
      li [class "nav-item dropdown", onMouseEnter (ProjectsShown True), onMouseLeave (ProjectsShown False)] [
        a [class "nav-link dropdown-toggle", role "button", ariaHaspopup True, ariaExpanded projectsShown, href "#"] [
          text "Projects"
        ],
        div [class "dropdown-menu", style [("display", if projectsShown then "block" else "none")]] (
          if List.isEmpty projects then
            [span [class "dropdown-item"] [text "You have no projects."]]
          else
            (projects |> List.map (\(Project name url) ->
              a [class "dropdown-item", href (Url.toString url)] [text name]
            ))
        )
      ]
    ]
  ]
