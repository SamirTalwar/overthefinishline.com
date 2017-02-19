module Test.App.Model exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Task
import Url

import App.Model exposing (Avatar (..), avatarLink)

tests : Tests
tests =
  [
    test "App.Model.avatarLink: a GitHub avatar link is parameterized with the correct size" (
      let
        avatar = GitHubAvatar (Url.parse "https://example.com/avatars/bob?v=3")
        expected = Url.parse "https://example.com/avatars/bob?v=3&s=24" |> Task.succeed
        actual = avatarLink avatar |> Task.succeed
      in
        assert actual (equals expected)
    )
  ]
