port module Main exposing (main)

import Arborist.Framework exposing (..)
import List

import Test.Moment
import Test.Server.Dashboard

tests : List Test
tests =
  List.concat [
    Test.Moment.tests,
    Test.Server.Dashboard.tests
  ]

port output : String -> Cmd message

main : Program Never
main = run tests output
