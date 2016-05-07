import Console
import ElmTest exposing (..)
import Task

import Test.GitHub.PullRequests

tests : Test
tests =
    suite "Over The Finish Line Dashboard tests" [
      Test.GitHub.PullRequests.tests
    ]

port runner : Signal (Task.Task x ())
port runner =
    Console.run (consoleRunner tests)
