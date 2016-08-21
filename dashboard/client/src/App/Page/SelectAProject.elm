module App.Page.SelectAProject exposing (html)

import App.Page.Html exposing (link)
import Html exposing (..)
import Html.Attributes exposing (class)

import App.Location as Location
import App.Model exposing (..)

html : Projects -> List (Html Message)
html projects =
  if List.isEmpty projects then
    [
      h1 [] [text "Welcome to Over The Finish Line."],
      h2 [] [link NavigateTo Location.NewProject [] [text "Start by creating a project."]]
    ]
  else
    [
      h1 [class "col-md-6"] [text "Please select a project."],
      div [class "col-md-6 project-list list-group"] (projects |> List.map (\(Project username name _) ->
        link NavigateTo (Location.Project username name) [class "list-group-item list-group-item-action"] [
          link NavigateTo (Location.EditProject username name) [class "tag tag-default pull-xs-right"] [
            text "Edit"
          ],
          text name
        ]
      ))
    ]
