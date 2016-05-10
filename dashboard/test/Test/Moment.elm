module Test.Moment (tests) where

import Result
import Task
import TestFramework exposing (Tests, test)

import Moment exposing (..)

tests : Tests
tests =
  [
    test "Moment.now: responds with the current time" (
      let
        now = Moment.now ()
        later = Task.sleep 100 `Task.andThen` Moment.now
        difference = Task.map2 Moment.durationBetween now later
        assertion = Task.map (\d -> d > 50 && d < 300) difference
      in
        (assertion, [
          ("Now", Task.map toString now),
          ("Later", Task.map toString later),
          ("Difference", Task.map toString difference)
        ])
    ),

    test "Moment.parse: parses and formats ISO-8601 strings in UTC" (
      let
        timestamp = "2019-07-20T20:18:30.000Z"
        parsed = Moment.parse timestamp
        formatted = Result.map Moment.format parsed
        assertion = Result.map ((==) timestamp) formatted
      in
        (Task.fromResult assertion,
         [("Timestamp", Task.succeed timestamp),
          ("Parsed", Task.fromResult <| Result.map toString parsed),
          ("Formatted", Task.fromResult <| Result.map toString formatted)])
    ),

    test "Moment.compare: LT" (
      let
        a = Moment.parse "2019-07-01T16:45:12.000Z"
        b = Moment.parse "2019-07-02T08:03:09.000Z"
        assertion = Result.map ((==) LT) <| Result.map2 Moment.compare a b
      in
        (Task.fromResult assertion,
         [("A", Task.fromResult <| Result.map toString a),
          ("B", Task.fromResult <| Result.map toString b)])
    ),

    test "Moment.compare: GT" (
      let
        a = Moment.parse "2019-07-30T15:45:00.000Z"
        b = Moment.parse "2019-07-30T10:50:00.000Z"
        assertion = Result.map ((==) GT) <| Result.map2 Moment.compare a b
      in
        (Task.fromResult assertion,
         [("A", Task.fromResult <| Result.map toString a),
          ("B", Task.fromResult <| Result.map toString b)])
    ),

    test "Moment.compare: EQ" (
      let
        a = Moment.parse "2019-09-01T12:34:56.789Z"
        b = Moment.parse "2019-09-01T12:34:56.789Z"
        assertion = Result.map ((==) EQ) <| Result.map2 Moment.compare a b
      in
        (Task.fromResult assertion,
         [("A", Task.fromResult <| Result.map toString a),
          ("B", Task.fromResult <| Result.map toString b)])
    ),

    test "Moment.durationBetween: subtracts two moments" (
      let
        a = Moment.parse "2019-03-14T19:00:00.000Z"
        b = Moment.parse "2019-06-30T07:00:00.000Z"
        expected = 9288000000
        actual = Result.map2 Moment.durationBetween a b
        assertion = Result.map ((==) expected) actual
      in
        (Task.fromResult assertion,
         [("Expected", Task.succeed (toString expected)),
          ("Actual", Task.fromResult <| Result.map toString actual)])
    )
  ]
