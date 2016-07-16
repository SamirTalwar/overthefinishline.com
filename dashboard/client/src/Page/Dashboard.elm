module Page.Dashboard exposing (html)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import List
import Moment exposing (Moment)

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
            let
              old = Moment.durationBetween pullRequest.updatedAt now > Moment.durationOf 3 Moment.Hours
            in
              tr [] [
                td [class "link"] [
                  a [href pullRequest.repository.link] [
                    text (pullRequest.repository.owner ++ " / " ++ pullRequest.repository.name)]],
                td [class "link"] [
                  a [href pullRequest.link] [
                    text ("#" ++ (toString pullRequest.number))]],
                td [class "link"] [
                  a [href pullRequest.link] [
                    text pullRequest.title]],
                td (if old then [class "old"] else []) [
                  text ("updated " ++ pullRequest.updatedAt `Moment.from` now)]
              ]
          )
      )
    ]
  ]
