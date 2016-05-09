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
        True -> name ++ " PASSED"
        False -> name ++ " FAILED")
    `Task.onError` (\error -> Task.succeed (name ++ " FAILED:\n  " ++ error))
    `Task.andThen` Signal.send mailbox.address)
  |> Task.sequence
  |> Task.map (\_ -> ())
