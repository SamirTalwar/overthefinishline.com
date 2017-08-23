module App.Error exposing (..)

import App.Location exposing (Location)


type Error
    = UnknownError String
    | NotFound
    | MissingPage Location
    | FailureToConnect
    | UnexpectedResponse String
