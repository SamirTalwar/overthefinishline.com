module Test.Server.Me exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeString)
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

        expected : Task Error (Response User)
        expected = Task.succeed (Response (User "_why" [
          Project "Camping" "/projects/camping",
          Project "Hpricot" "/projects/hpricot",
          Project "RedCloth" "/projects/redcloth",
          Project "Shoes" "/project/shoes"
        ]))

        actual : Task Error (Response User)
        actual = fetch get
      in
        assert actual (equals expected)
    ),

    test "Server.Me.fetch: recognises an unauthenticated response" (
      let
        get : Decoder a -> String -> Task Http.Error a
        get decoder url =
          if url == "/me"
            then Task.fromResult (decodeString decoder unauthenticatedJson)
                   |> Task.mapError Http.UnexpectedPayload
            else Task.fail (Http.BadResponse 404 "Not Found")

        expected : Task Error (Response User)
        expected = Task.succeed UnauthenticatedResponse

        actual : Task Error (Response User)
        actual = fetch get
      in
        assert actual (equals expected)
    )
  ]

authenticatedUserJson : String
authenticatedUserJson =
  """
    {
      "state": "Authenticated",
      "username": "_why",
      "projects": [
          { "name": "Camping", "link": "/projects/camping" },
          { "name": "Hpricot", "link": "/projects/hpricot" },
          { "name": "RedCloth", "link": "/projects/redcloth" },
          { "name": "Shoes", "link": "/project/shoes" }
        ]
    }
  """

unauthenticatedJson : String
unauthenticatedJson =
  """
    {
      "state": "Unauthenticated"
    }
  """
