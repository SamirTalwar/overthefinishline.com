port module Main exposing (main)

import Arborist.Framework exposing (..)
import List

import Test.Moment
import Test.Server.Dashboard
import Test.Server.Me

tests : List Test
tests =
  List.concat [
    Test.Moment.tests,
    Test.Server.Dashboard.tests,
    Test.Server.Me.tests
  ]

port output : String -> Cmd message

main : Program Never
main = run tests output
