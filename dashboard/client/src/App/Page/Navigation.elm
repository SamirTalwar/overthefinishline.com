module App.Page.Navigation exposing (html)

import App.Page.Html exposing (..)
import Html exposing (..)
import Html.Attributes exposing (alt, class, style)
import Html.Events exposing (..)

import App.Model exposing (Me (..), User (..), Project (..), avatarLink)
import App.Navigation as Navigation exposing (..)
import App.Urls exposing (..)

html : Me -> State -> Html Navigation.Message
html (Me (User username avatar) projects) (ProjectsShown projectsShown) =
  nav [class "navbar navbar-dark bg-inverse"] [
    link NavigateTo root [class "navbar-brand"] [text "Over The Finish Line"],
    ul [class "nav navbar-nav"]  [
      li [class "nav-item"] [
        a [class "nav-link", emptyHref] [
          span [class "avatar"] [img [src (avatarLink avatar), alt ""] []],
          span [class "username"] [text username]
        ]
      ],
      li [class "nav-item dropdown", onMouseEnter ShowProjects, onMouseLeave HideProjects] [
        a [class "nav-link dropdown-toggle", role "button", ariaHaspopup True, ariaExpanded projectsShown, emptyHref] [
          text "Projects"
        ],
        div [class "dropdown-menu", style [("display", if projectsShown then "block" else "none")]] (
          (projects |> List.map (\(Project name url) ->
            link NavigateTo url [class "dropdown-item"] [text name]
          )) ++
          [link NavigateTo newProject [class "dropdown-item"] [text "Create a project"]]
        )
      ]
    ]
  ]
