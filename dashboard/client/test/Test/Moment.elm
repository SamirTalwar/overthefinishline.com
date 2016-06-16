module Test.Moment exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Result
import Process
import Task

import Moment exposing (..)

tests : Tests
tests =
  [
    test "Moment.now: responds with the current time" (
      let
        now = Moment.now ()
        later = Process.sleep 100 `Task.andThen` Moment.now
        difference = Task.map2 Moment.durationBetween now later
      in
        assert difference (isIntBetween (Task.succeed 50) (Task.succeed 300))
    ),

    test "Moment.parse: parses and formats ISO-8601 strings in UTC" (
      let
        timestamp = "2019-07-20T20:18:30.000Z"
        parsed = Moment.parse timestamp
        formatted = Result.map Moment.format parsed
        assertion = Result.map ((==) timestamp) formatted
      in
        assert (Task.fromResult formatted) (equals (Task.succeed timestamp))
    ),

    test "Moment.compare: LT" (
      let
        a = Moment.parse "2019-07-01T16:45:12.000Z"
        b = Moment.parse "2019-07-02T08:03:09.000Z"
        comparison = Result.map2 Moment.compare a b
      in
        assert (Task.fromResult comparison) (equals (Task.succeed LT))
    ),

    test "Moment.compare: GT" (
      let
        a = Moment.parse "2019-07-30T15:45:00.000Z"
        b = Moment.parse "2019-07-30T10:50:00.000Z"
        comparison = Result.map2 Moment.compare a b
      in
        assert (Task.fromResult comparison) (equals (Task.succeed GT))
    ),

    test "Moment.compare: EQ" (
      let
        a = Moment.parse "2019-09-01T12:34:56.789Z"
        b = Moment.parse "2019-09-01T12:34:56.789Z"
        comparison = Result.map2 Moment.compare a b
      in
        assert (Task.fromResult comparison) (equals (Task.succeed EQ))
    ),

    test "Moment.durationOf: calculates durations in hours" (
      let
        expected = 21600000 |> Task.succeed
        actual = Moment.durationOf 6 Hours |> Task.succeed
      in
        assert actual (equals expected)
    ),

    test "Moment.durationBetween: calcluates the difference between two moments in milliseconds" (
      let
        a = Moment.parse "2019-03-14T19:00:00.000Z"
        b = Moment.parse "2019-06-30T07:00:00.000Z"
        expected = 9288000000 |> Task.succeed
        actual = Result.map2 Moment.durationBetween a b |> Task.fromResult
      in
        assert actual (equals expected)
    ),

    test "Moment.from: subtracts two moments in a fuzzy way" (
      let
        a = Moment.parse "2022-04-05T15:00:00.000Z"
        b = Moment.parse "2022-04-09T08:00:00.000Z"
        expected = "4 days ago" |> Task.succeed
        actual = Result.map2 Moment.from a b |> Task.fromResult
      in
        assert actual (equals expected)
    )
  ]
