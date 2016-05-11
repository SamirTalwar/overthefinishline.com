port module TestFramework exposing (
    Assertion,
    Name,
    Test,
    Tests,
    run,
    test
  )

import List
import String
import Task exposing (Task)

import Native.TestFramework

type alias Name = String

type alias Assertion = Task String Bool

type alias FailureMessage = (String, Task String String)

type Test = Test { name : Name, assertion : Assertion, failureMessages : List FailureMessage }

type alias Tests = List Test

test : Name -> (Assertion, List FailureMessage) -> Test
test name (assertion, failureMessages) =
  Test { name = name, assertion = assertion, failureMessages = failureMessages }

run : List Test -> Cmd message
run tests =
  (flip List.map) tests (\(Test { name, assertion, failureMessages }) ->
    assertion
    `Task.andThen` (\testPassed ->
      if testPassed
        then Task.succeed (passed name)
        else failed name failureMessages)
    `Task.onError` (\error -> failed name ([("Error", Task.fail error)] ++ failureMessages))
    |> Task.perform identity Native.TestFramework.runTest
  )
  |> Cmd.batch

passed name = green (name ++ " PASSED")

failed name failureMessages =
  failureMessages
    |> List.map (\(key, valueTask) -> Task.map (renderMessage key) (Task.toResult valueTask))
    |> Task.sequence
    |> Task.map (String.join "")
    |> Task.map (\messages -> name ++ " FAILED" ++ messages)
    |> Task.map red

renderMessage key value =
  "\n  " ++ key ++ ":\n  " ++ case value of
    Ok ok -> ok
    Err err -> "Error: " ++ err

green string = "\x1b[32m" ++ string ++ reset

red string = "\x1b[31m" ++ string ++ reset

reset = "\x1b[0m"
