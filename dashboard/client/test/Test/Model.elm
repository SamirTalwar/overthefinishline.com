module Test.Model exposing (tests)

import Arborist.Framework exposing (..)
import Arborist.Matchers exposing (..)
import Erl
import Task

import Model exposing (Avatar (..), avatarLink)

tests : Tests
tests =
  [
    test "Model.avatarLink: a GitHub avatar link is parameterized with the correct size" (
      let
        avatar = GitHubAvatar (Erl.parse "https://example.com/avatars/bob?v=3")
        expected = "https://example.com/avatars/bob?s=32&v=3" |> Task.succeed
        actual = avatarLink avatar |> Task.succeed
      in
        assert actual (equals expected)
    )
  ]
