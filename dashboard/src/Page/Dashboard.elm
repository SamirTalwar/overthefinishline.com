module Page.Dashboard where

import Html exposing (..)
import Html.Attributes exposing (class)
import List
import Moment

html { now, pullRequests } =
  [
    section [class "summary"] [
      span [] [text "You have"],
      span [class "display-3"] [text (toString <| List.length pullRequests)],
      span [] [text "open pull requests."]
    ],
    section [class "details"] [
      table [class "table"] (
        pullRequests
          |> List.take 3
          |> List.map (\pullRequest ->
            tr [] [
              td [] [text (pullRequest.repository.owner ++ " / " ++ pullRequest.repository.repository)],
              td [] [text ("#" ++ (toString pullRequest.number))],
              td [] [text pullRequest.title],
              td [] [text (pullRequest.updatedAt `Moment.from` now)]
            ]
          )
      )
    ]
  ]
