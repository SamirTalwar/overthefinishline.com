module Tests exposing (tests)

import Arborist.Framework exposing (..)
import List

import Test.App.Model
import Test.App.Server.Dashboard
import Test.App.Server.Me
import Test.Moment

tests : List Test
tests =
  List.concat [
    Test.App.Model.tests,
    Test.App.Server.Dashboard.tests,
    Test.App.Server.Me.tests,
    Test.Moment.tests
  ]
