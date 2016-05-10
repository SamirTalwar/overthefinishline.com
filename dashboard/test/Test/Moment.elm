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
    ),

    test "Moment.compare: LT" (
      let
        a = Moment.parse "2019-07-01T16:45:12.000Z"
        b = Moment.parse "2019-07-02T08:03:09.000Z"
        assertion = Result.map ((==) LT) <| Result.map2 Moment.compare a b
      in
        (Task.fromResult assertion,
         [("A", toString a),
          ("B", toString b)])
    ),

    test "Moment.compare: GT" (
      let
        a = Moment.parse "2019-07-30T15:45:00.000Z"
        b = Moment.parse "2019-07-30T10:50:00.000Z"
        assertion = Result.map ((==) GT) <| Result.map2 Moment.compare a b
      in
        (Task.fromResult assertion,
         [("A", toString a),
          ("B", toString b)])
    ),

    test "Moment.compare: EQ" (
      let
        a = Moment.parse "2019-09-01T12:34:56.789Z"
        b = Moment.parse "2019-09-01T12:34:56.789Z"
        assertion = Result.map ((==) EQ) <| Result.map2 Moment.compare a b
      in
        (Task.fromResult assertion,
         [("A", toString a),
          ("B", toString b)])
    )
  ]
