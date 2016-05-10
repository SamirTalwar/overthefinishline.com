import List
import Task
import TestFramework exposing (..)

import Test.GitHub.PullRequests
import Test.Moment

tests : List Test
tests =
  List.concat [
    Test.GitHub.PullRequests.tests,
    Test.Moment.tests
  ]

port run : Task.Task x ()
port run = TestFramework.run tests

port display : Signal String
port display = TestFramework.display
