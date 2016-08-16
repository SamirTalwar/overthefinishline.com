module Page.SelectAProject exposing (html)

import Html exposing (..)
import Html.Attributes exposing (class)
import Page.Attributes exposing (href)
import Url

import Model exposing (..)

html : Projects -> List (Html a)
html projects =
  if List.isEmpty projects then
    [
      h1 [] [text "Welcome to Over The Finish Line."],
      h2 [] [a [href (Url.parse "/projects/new")] [text "Start by creating a project."]]
    ]
  else
    [
      h1 [] [text "Please select a project."],
      div [class "project-list list-group"] (projects |> List.map (\(Project name link) ->
        a [class "list-group-item list-group-item-action", href link] [text name]
      ))
    ]
