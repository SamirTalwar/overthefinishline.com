module Page.Dashboard where

import Html exposing (..)
import List

html { pullRequests } =
  div [] (List.map (\pullRequest -> p [] [text pullRequest.title]) pullRequests)
