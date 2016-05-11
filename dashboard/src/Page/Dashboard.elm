module Page.Dashboard where

import Html exposing (..)
import Html.Attributes exposing (class, href)
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
              td [class "link"] [a [href pullRequest.link] [text ("#" ++ (toString pullRequest.number))]],
              td [class "link"] [a [href pullRequest.link] [text pullRequest.title]],
              td [] [text ("updated " ++ pullRequest.updatedAt `Moment.from` now)]
            ]
          )
      )
    ]
  ]
