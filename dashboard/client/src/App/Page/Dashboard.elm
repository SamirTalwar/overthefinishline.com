module App.Page.Dashboard exposing (html)

import App.Page.Html exposing (href, role)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import List
import Moment exposing (Moment)
import Url

import App.Model exposing (..)
import App.Http as Http

html : Http.Failures -> Dashboard -> List (Html Message)
html failures (Dashboard _ now pullRequests) =
  let
    pullRequestCount = List.length pullRequests
  in
    [
      failureHtml failures,
      section [class "summary"] [
        span [] [text "You have"],
        span [class "display-3"] [text (toString pullRequestCount)],
        span [] [text (if pullRequestCount == 1 then "open pull request." else "open pull requests.")]
      ],
      section [class "details"] [
        table [class "table"] (
          pullRequests |> List.map (\pullRequest ->
            let
              old = Moment.durationBetween pullRequest.updatedAt now > Moment.durationOf 3 Moment.Hours
            in
              tr [] [
                td [class "link repository"] [
                  a [href pullRequest.repository.url] [
                    text (pullRequest.repository.owner ++ " / " ++ pullRequest.repository.name)]],
                td [class "link pr-number"] [
                  a [href pullRequest.url] [
                    text ("#" ++ (toString pullRequest.number))]],
                td [class "link pr-title"] [
                  a [href pullRequest.url] [
                    text pullRequest.title]],
                td ([classList [("timestamp", True), ("old", old)]]) [
                  text ("updated " ++ pullRequest.updatedAt `Moment.from` now)]
              ]
          )
        )
      ]
    ]

failureHtml : Http.Failures -> Html a
failureHtml failures =
  div [class "failures"] (failures |> List.map (\failure ->
    div [class "alert alert-danger", role "alert"] (failureText failure)
  ))

failureText : Http.Failure -> List (Html a)
failureText failure =
  case failure of
    Http.RequestFailure url message ->
      [text "Failed to get a response for ", a [href url] [text (Url.toString url)], text ".", br [] [],
       text "Error: ", text message]
