module App.Page.SelectAProject exposing (html)

import App.Page.Html exposing (href, link)
import Html exposing (..)
import Html.Attributes exposing (class)

import App.Model exposing (..)
import App.Urls exposing (..)

html : Projects -> List (Html Message)
html projects =
  if List.isEmpty projects then
    [
      h1 [] [text "Welcome to Over The Finish Line."],
      h2 [] [link NavigateTo newProject [] [text "Start by creating a project."]]
    ]
  else
    [
      h1 [] [text "Please select a project."],
      div [class "project-list list-group"] (projects |> List.map (\(Project name url) ->
        link NavigateTo url [class "list-group-item list-group-item-action"] [text name]
      ))
    ]
