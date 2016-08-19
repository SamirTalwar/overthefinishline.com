module App.Page.Navigation exposing (signedIn, signedOut)

import App.Page.Html exposing (..)
import Html exposing (..)
import Html.Attributes exposing (alt, class, style)
import Html.Events exposing (..)

import App.Location as Location
import App.Model exposing (Me (..), User (..), Project (..), avatarLink)
import App.Navigation exposing (..)

signedIn : Me -> State -> Html Message
signedIn (Me (User username avatar) projects) (ProjectsShown projectsShown) =
  navigationBar [
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
        [link NavigateTo Location.NewProject [class "dropdown-item"] [text "Create a project"]]
      )
    ]
  ]

signedOut : Html Message
signedOut = navigationBar []

navigationBar : List (Html Message) -> Html Message
navigationBar items =
  nav [class "navbar navbar-dark bg-inverse"] [
    title,
    ul [class "nav navbar-nav"] items
  ]

title : Html Message
title = link NavigateTo Location.Home [class "navbar-brand"] [text "Over The Finish Line"]
