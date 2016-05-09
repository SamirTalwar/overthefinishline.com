import Task
import TestFramework exposing (..)

import Test.GitHub.PullRequests

tests : List Test
tests = Test.GitHub.PullRequests.tests

port run : Task.Task x ()
port run = TestFramework.run tests

port display : Signal String
port display = TestFramework.display
