module Page.Dashboard where

import Html exposing (..)
import List

html prs =
  div [] (List.map (\pr -> p [] [text pr.title]) prs)
