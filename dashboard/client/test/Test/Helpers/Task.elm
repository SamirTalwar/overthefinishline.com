module Test.Helpers.Task exposing (taskFromResult)

import Result exposing (Result)
import Task exposing (Task)

taskFromResult : Result x a -> Task x a
taskFromResult result =
  case result of
    Result.Ok value -> Task.succeed value
    Result.Err error -> Task.fail error
