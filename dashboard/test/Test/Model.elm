module Test.Model (tests) where

import Moment
import Result
import Task
import TestFramework exposing (Tests, test)

import Model exposing (..)

tests : Tests
tests =
  [
    test "Model.dashboard: gets the pull requests, least-recently-updated first" (
      let
        expectedPullRequests = Model.pullRequests [
          {
            repository = { owner = "people", repository = "alice" },
            number = 67,
            title = "Upgraded to v18.0.",
            updatedAt = Moment.parse "2016-05-01T12:00:00Z"
          },
          {
            repository = { owner = "people", repository = "bob" },
            number = 107,
            title = "Add some confidence so Bob can talk to Alice.",
            updatedAt = Moment.parse "2016-05-07T20:00:00Z"
          },
          {
            repository = { owner = "people", repository = "carol" },
            number = 45,
            title = "Develop an intense hatred of tomatoes.",
            updatedAt = Moment.parse "2016-05-19T06:00:00Z"
          },
          {
            repository = { owner = "people", repository = "alice" },
            number = 78,
            title = "Graduate.",
            updatedAt = Moment.parse "2016-06-15T09:00:00Z"
          },
          {
            repository = { owner = "people", repository = "carol" },
            number = 50,
            title = "Get back into zines.",
            updatedAt = Moment.parse "2016-08-11T23:55:00Z"
          }
        ]
        expected = Result.map (\input -> Dashboard { pullRequests = input }) expectedPullRequests
        actual = Result.map (\input -> createDashboard { pullRequests = input }) pullRequests
        assertion = Result.map2 (==) expected actual
      in
        (Task.fromResult assertion, [
          ("Expected", Task.fromResult <| Result.map toString expected),
          ("Actual", Task.fromResult <| Result.map toString actual)
        ])
    )
  ]

pullRequests : Result String PullRequests
pullRequests =
  Model.pullRequests [
    {
      repository = { owner = "people", repository = "alice" },
      number = 67,
      title = "Upgraded to v18.0.",
      updatedAt = Moment.parse "2016-05-01T12:00:00Z"
    },
    {
      repository = { owner = "people", repository = "alice" },
      number = 78,
      title = "Graduate.",
      updatedAt = Moment.parse "2016-06-15T09:00:00Z"
    },
    {
      repository = { owner = "people", repository = "bob" },
      number = 107,
      title = "Add some confidence so Bob can talk to Alice.",
      updatedAt = Moment.parse "2016-05-07T20:00:00Z"
    },
    {
      repository = { owner = "people", repository = "carol" },
      number = 45,
      title = "Develop an intense hatred of tomatoes.",
      updatedAt = Moment.parse "2016-05-19T06:00:00Z"
    },
    {
      repository = { owner = "people", repository = "carol" },
      number = 50,
      title = "Get back into zines.",
      updatedAt = Moment.parse "2016-08-11T23:55:00Z"
    }
  ]
