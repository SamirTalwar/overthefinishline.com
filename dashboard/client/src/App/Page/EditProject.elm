module App.Page.EditProject exposing (htmlForNewProject, htmlForExistingProject)

import App.List exposing (slideThrough)
import App.Page.Html exposing (formFor, link)
import Html exposing (..)
import Html.Attributes exposing (class, for, name, placeholder, required, type', value)
import Html.Events exposing (onInput)
import String

import App.Location as Location
import App.Model exposing (..)

htmlForNewProject : List Name -> List (Html Message)
htmlForNewProject repositoryNames =
  [
    div [class "col-md-6"] [
      h1 [] [text "Create a new project"],
      p [] [text "Each project looks at one or more GitHub repositories, and creates a dashboard so you can monitor work in progress."],
      p [] [text "At the moment, we just measure pull requests, but more is coming soon!"],
      formFor NavigateTo Location.NewProject [] (formElements Nothing repositoryNames)
    ]
  ]

htmlForExistingProject : Project -> List (Html Message)
htmlForExistingProject (Project username projectName repositoryNames) =
  [
    div [class "col-md-6"] [
      h1 [] [text "Edit your project"],
      p [] [text ("You're currently editing " ++ toString projectName ++ ".")],
      formFor NavigateTo (Location.EditProject username projectName) [] (formElements (Just projectName) repositoryNames)
    ]
  ]

formElements : Maybe Name -> List Name -> List (Html Message)
formElements projectName repositoryNames =
  [
    div [class "form-group"] [
      label [for "project-name"] [text "Project name"],
      input ([type' "text", class "form-control", name "project-name", required True] ++ optionally value projectName) []
    ],
    div [class "form-group"] <| [
      p [] [text "Repositories"]
    ] ++ slideThrough repositoryField (List.filter (not << String.isEmpty) repositoryNames ++ [""]),
    button [type' "submit", class "btn btn-primary"] [text "Submit"]
  ]

repositoryField : List Name -> Name -> List Name -> Html Message
repositoryField before repositoryName after =
  p [] [
    input
      [type' "text", class "form-control", name "repository-names[]", placeholder "elm-lang/core", value repositoryName,
       onInput (\newName -> UpdateRepositoryNames (before ++ [newName] ++ after))]
      []
  ]

optionally : (a -> b) -> Maybe a -> List b
optionally f maybe =
  case maybe of
    Nothing -> []
    Just value -> [f value]
