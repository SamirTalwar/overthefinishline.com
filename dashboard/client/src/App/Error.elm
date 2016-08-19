module App.Error exposing (..)

type Error =
    UnknownError String
  | MissingPage String
  | FailureToConnect
  | UnexpectedResponse String
