module HttpX exposing (..)

import Http
import Json.Decode exposing (Decoder)
import Task exposing (Task)

import Error exposing (Error)

type alias Get a = Decoder a -> String -> Task Http.Error a

handleError : Http.Error -> Error
handleError error = case error of
  Http.Timeout -> Error.FailureToConnect
  Http.NetworkError -> Error.FailureToConnect
  Http.UnexpectedPayload payload -> Error.UnexpectedResponse payload
  Http.BadResponse status message -> Error.UnexpectedResponse (toString status ++ " " ++ message)
