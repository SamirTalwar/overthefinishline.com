module Test.Server.Me exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeString)
import Moment
import Task exposing (Task)

import Server.Me exposing (..)
import Error exposing (..)
import Model exposing (..)

tests : Tests
tests =
  [
    test "Server.Me.fetch: fetches the user" (
      let
        get : Decoder a -> String -> Task Http.Error a
        get decoder url =
          if url == "/me"
            then Task.fromResult (decodeString decoder authenticatedUserJson)
                   |> Task.mapError Http.UnexpectedPayload
            else Task.fail (Http.BadResponse 404 "Not Found")

        expected : Task Error User
        expected = Task.succeed (AuthenticatedUser { username = "_why", projects = [
          { name = "Camping", link = "/projects/camping" },
          { name = "Hpricot", link = "/projects/hpricot" },
          { name = "RedCloth", link = "/projects/redcloth" },
          { name = "Shoes", link = "/project/shoes" }
        ]})

        actual : Task Error User
        actual = fetch get
      in
        assert actual (equals expected)
    ),

    test "Server.Me.fetch: recognises an unauthenticated user" (
      let
        get : Decoder a -> String -> Task Http.Error a
        get decoder url =
          if url == "/me"
            then Task.fromResult (decodeString decoder unauthenticatedUserJson)
                   |> Task.mapError Http.UnexpectedPayload
            else Task.fail (Http.BadResponse 404 "Not Found")

        expected : Task Error User
        expected = Task.succeed UnauthenticatedUser

        actual : Task Error User
        actual = fetch get
      in
        assert actual (equals expected)
    )
  ]

authenticatedUserJson =
  """
    {
      "tag": "AuthenticatedUser",
      "username": "_why",
      "projects": [
          { "name": "Camping", "link": "/projects/camping" },
          { "name": "Hpricot", "link": "/projects/hpricot" },
          { "name": "RedCloth", "link": "/projects/redcloth" },
          { "name": "Shoes", "link": "/project/shoes" }
        ]
    }
  """

unauthenticatedUserJson =
  """
    {
      "tag": "UnauthenticatedUser"
    }
  """
