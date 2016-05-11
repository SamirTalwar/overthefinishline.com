module Main exposing (main)

import Html
import Html.App exposing (program)
import List
import TestFramework exposing (..)

import Test.GitHub.PullRequests
import Test.Model
import Test.Moment

tests : List Test
tests =
  List.concat [
    Test.GitHub.PullRequests.tests,
    Test.Model.tests,
    Test.Moment.tests
  ]

main : Program Never
main =
  program {
    init = ((), run tests),
    update = \message model -> (model, Cmd.none),
    view = \model -> Html.div [] [],
    subscriptions = always Sub.none
  }
