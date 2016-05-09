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
import Task exposing (Task)

type alias Name = String

type alias Assertion = Task String Bool

type Test = Test Name Assertion

test : Name -> Assertion -> Test
test = Test

mailbox : Signal.Mailbox String
mailbox = Signal.mailbox "tests"

display : Signal String
display = mailbox.signal

run : List Test -> Task x ()
run tests =
  (flip List.map) tests (\(Test name assertion) ->
    (flip Task.map) assertion (\result -> case result of
        True -> green (name ++ " PASSED")
        False -> red (name ++ " FAILED"))
    `Task.onError` (\error -> Task.succeed (red (name ++ " FAILED:\n  " ++ error)))
    `Task.andThen` Signal.send mailbox.address)
  |> Task.sequence
  |> Task.map (\_ -> ())

green string = "\x1b[32m" ++ string ++ reset

red string = "\x1b[31m" ++ string ++ reset

reset = "\x1b[0m"
