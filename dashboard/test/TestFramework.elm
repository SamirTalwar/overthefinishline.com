module TestFramework (
  Assertion,
  Name,
  Test,
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

type alias FailureMessage = (String, String)

type Test = Test { name : Name, assertion : Assertion, failureMessages : List FailureMessage }

test : Name -> (Assertion, List FailureMessage) -> Test
test name (assertion, failureMessages) =
  Test { name = name, assertion = assertion, failureMessages = failureMessages }

mailbox : Signal.Mailbox String
mailbox = Signal.mailbox "tests"

display : Signal String
display = mailbox.signal

run : List Test -> Task x ()
run tests =
  (flip List.map) tests (\(Test { name, assertion, failureMessages }) ->
    (flip Task.map) assertion (\result -> case result of
        True -> passed name
        False -> failed name failureMessages)
    `Task.onError` (\error -> Task.succeed (failed name ([("Error", toString error)] ++ failureMessages)))
    `Task.andThen` Signal.send mailbox.address)
  |> Task.sequence
  |> Task.map (\_ -> ())

passed name = green (name ++ " PASSED")

failed name failureMessages =
  red <| name ++ " FAILED" ++ String.join "" (List.map renderMessage failureMessages)

renderMessage (key, value) = "\n  " ++ key ++ ":\n  " ++ value

green string = "\x1b[32m" ++ string ++ reset

red string = "\x1b[31m" ++ string ++ reset

reset = "\x1b[0m"
