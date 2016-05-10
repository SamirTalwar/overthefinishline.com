module Page.Dashboard where

import Html exposing (..)
import Html.Attributes exposing (class)
import List

html { pullRequests } =
  [
    section [class "summary"] [
      span [] [text "You have"],
      span [class "display-3"] [text (toString <| List.length pullRequests)],
      span [] [text "open pull requests."]
    ],
    section [class "details"]
      <| List.map (\pullRequest -> p [] [text pullRequest.title]) <| List.take 3 pullRequests
  ]
