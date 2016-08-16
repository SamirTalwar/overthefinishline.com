module App.Http exposing (..)

import Http
import Json.Decode exposing (Decoder)
import Task exposing (Task)

import App.Error exposing (..)

type alias Get a = Decoder a -> String -> Task Http.Error a

handleError : Http.Error -> Error
handleError error = case error of
  Http.Timeout -> FailureToConnect
  Http.NetworkError -> FailureToConnect
  Http.UnexpectedPayload payload -> UnexpectedResponse payload
  Http.BadResponse status message -> UnexpectedResponse (toString status ++ " " ++ message)
