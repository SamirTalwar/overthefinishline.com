module TestFramework (
  Assertion,
  Name,
  Test,
  Tests,
  display,
  run,
  test
  ) where

import List
import Signal
import String
import Task exposing (Task)

type alias Name = String

type alias Assertion = Task String Bool

type alias FailureMessage = (String, Task String String)

type Test = Test { name : Name, assertion : Assertion, failureMessages : List FailureMessage }

type alias Tests = List Test

test : Name -> (Assertion, List FailureMessage) -> Test
test name (assertion, failureMessages) =
  Test { name = name, assertion = assertion, failureMessages = failureMessages }

mailbox : Signal.Mailbox String
mailbox = Signal.mailbox "tests"

display : Signal String
display = mailbox.signal

run : List Test -> Task String ()
run tests =
  (flip List.map) tests (\(Test { name, assertion, failureMessages }) ->
    assertion `Task.andThen` (\result ->
      case result of
        True -> Task.succeed (passed name)
        False -> failed name failureMessages)
    `Task.onError` (\error -> failed name ([("Error", Task.fail error)] ++ failureMessages))
    `Task.andThen` Signal.send mailbox.address)
  |> Task.sequence
  |> Task.map (always ())

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
