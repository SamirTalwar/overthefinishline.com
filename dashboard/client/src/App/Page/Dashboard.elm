module App.Page.Dashboard exposing (html)

import App.Page.Html exposing (href)
import Html exposing (..)
import Html.Attributes exposing (class)
import List
import Moment exposing (Moment)

import App.Model exposing (..)

html : Dashboard -> List (Html Message)
html (Dashboard _ now pullRequests) =
  let
    pullRequestCount = List.length pullRequests
  in
    [
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
                td [class "link"] [
                  a [href pullRequest.repository.url] [
                    text (pullRequest.repository.owner ++ " / " ++ pullRequest.repository.name)]],
                td [class "link"] [
                  a [href pullRequest.url] [
                    text ("#" ++ (toString pullRequest.number))]],
                td [class "link"] [
                  a [href pullRequest.url] [
                    text pullRequest.title]],
                td (if old then [class "old"] else []) [
                  text ("updated " ++ pullRequest.updatedAt `Moment.from` now)]
              ]
          )
        )
      ]
    ]
