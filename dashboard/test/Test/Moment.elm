module Test.Moment (tests) where

import Result
import Task
import TestFramework exposing (test)

import Moment exposing (..)

tests =
  [
    test "Moment.parse: parses and formats ISO-8601 strings in UTC" (
      let
        timestamp = "2019-07-20T20:18:30.000Z"
        parsed = Moment.parse timestamp
        formatted = Result.map Moment.format parsed
        assertion = Result.map ((==) timestamp) formatted
      in
        (Task.fromResult assertion,
         [("Timestamp", timestamp),
          ("Parsed", (toString parsed)),
          ("Formatted", (toString formatted))])
    )
  ]
