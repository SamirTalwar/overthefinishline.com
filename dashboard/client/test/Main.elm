port module Main exposing (main)

import Arborist.Framework exposing (..)
import List

import Test.GitHub.PullRequests
import Test.Model
import Test.Moment
import Test.Server.Dashboard

tests : List Test
tests =
  List.concat [
    Test.GitHub.PullRequests.tests,
    Test.Model.tests,
    Test.Moment.tests,
    Test.Server.Dashboard.tests
  ]

port output : String -> Cmd message

main : Program Never
main = run tests output
