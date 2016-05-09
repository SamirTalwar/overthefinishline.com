module Styles (
    Classes (..),
    namespace,
    css
  ) where

import Css exposing (..)
import Css.Elements exposing (body)
import Css.Namespace

type Classes = PullRequests

namespace = "overTheFinishLine"

css =
  (stylesheet << Css.Namespace.namespace namespace)
    [
      (.) PullRequests [
        margin (px 50)
      ]
    ]
