import List
import Task
import TestFramework exposing (..)

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

port run : Task.Task String ()
port run = TestFramework.run tests

port display : Signal String
port display = TestFramework.display
