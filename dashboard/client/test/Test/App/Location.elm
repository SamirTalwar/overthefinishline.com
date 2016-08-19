module Test.App.Location exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Task

import App.Location exposing (..)
import Navigation

tests : Tests
tests =
  [
    test "Location: parses the root URL" (
      let
        expected = Home |> Task.succeed
        actual = parser (locationWithPath "/") |> Task.succeed
      in
        assert actual (equals expected)
    ),

    test "Location: parses the new project URL" (
      let
        expected = NewProject |> Task.succeed
        actual = parser (locationWithPath "/projects") |> Task.succeed
      in
        assert actual (equals expected)
    ),

    test "Location: fails when the URL could not be parsed" (
      let
        actual = parser (locationWithPath "/not/gonna/work")
      in
        case actual of
          Error _ -> pass
          value -> failWith ("Got " ++ toString value)
    )
  ]

locationWithPath : String -> Navigation.Location
locationWithPath path =
  {
    href = "",
    host = "",
    hostname = "",
    protocol = "",
    origin = "",
    port_ = "",
    pathname = path,
    search = "",
    hash = "",
    username = "",
    password = ""
  }
