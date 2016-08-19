module Test.App.List exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Task exposing (Task)

import App.List exposing (slideThrough)

tests : Tests
tests =
  [
    test "App.List: slideThrough maps a list onto a function with before and after" (
      let
        expected = [
          ([], 1, [2, 3]),
          ([1],  2,  [3]),
          ([1, 2], 3, [])
        ] |> Task.succeed
        actual = slideThrough (,,) [1, 2, 3] |> Task.succeed
      in
        assert actual (equals expected)
    )
  ]
