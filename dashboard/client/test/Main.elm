port module Main exposing (main)

import Arborist.Framework exposing (..)
import Html
import Html.App exposing (program)
import List

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

port output : String -> Cmd message

main : Program Never
main =
  program {
    init = ((), run tests),
    update = \message model -> ((), output message),
    view = \model -> Html.div [] [],
    subscriptions = always Sub.none
  }
