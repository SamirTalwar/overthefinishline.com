module App.Error exposing (..)

import App.Location exposing (Location)

type Error =
    UnknownError String
  | MissingPage Location
  | FailureToConnect
  | UnexpectedResponse String
