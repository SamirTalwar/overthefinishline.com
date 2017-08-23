module App.Page.Navigation exposing (signedIn, signedOut)

import App.Page.Html exposing (..)
import Html exposing (..)
import Html.Attributes exposing (alt, class, height, method, style, type_, width)
import Html.Events exposing (..)

import App.Location as Location
import App.Model exposing (Me (..), User (..), Project (..), avatarLink, avatarSize)
import App.Navigation exposing (..)

signedIn : Me -> State -> Html Message
signedIn (Me (User username avatar) projects) (ProjectsShown projectsShown) =
  navigationBar [
    li [class "nav-item"] [
      a [class "nav-link", emptyHref] [
        span [class "avatar"] [img [src (avatarLink avatar), alt "", width avatarSize, height avatarSize] []],
        span [class "username"] [text username]
      ]
    ],
    li [class "nav-item dropdown", onMouseEnter ShowProjects, onMouseLeave HideProjects] [
      a [class "nav-link dropdown-toggle", role "button", ariaHaspopup True, ariaExpanded projectsShown, emptyHref] [
        text "Projects"
      ],
      div [class "dropdown-menu", style [("display", if projectsShown then "block" else "none")]] (
        (projects |> List.map (\(Project username name _) ->
          link NavigateTo (Location.Project username name) [class "dropdown-item"] [text name]
        ))
        ++ (if List.length projects == 0 then [] else [hr [] []])
        ++ [link NavigateTo Location.NewProject [class "dropdown-item"] [text "Create a project"]]
      )
    ]
  ] [
    li [class "nav-item"] [
      form [method "post", Html.Attributes.action "/sign-out"] [
        button [type_ "submit", class "btn"] [text "Sign out"]
      ]
    ]
  ]

signedOut : Html Message
signedOut =
  navigationBar [] [
    li [class "nav-item"] [
      form [method "post", Html.Attributes.action "/authentication/by/github"] [
        button [type_ "submit", class "btn"] [text "Sign in with GitHub"]
      ]
    ]
  ]

navigationBar : List (Html Message) -> List (Html Message) -> Html Message
navigationBar leftItems rightItems =
  nav [class "navbar navbar-expand-lg navbar-dark bg-dark"] [
    title,
    ul [class "nav navbar-nav mr-auto"] leftItems,
    ul [class "nav navbar-nav"] rightItems
  ]

title : Html Message
title = link NavigateTo Location.Home [class "navbar-brand"] [text "Over The Finish Line"]
