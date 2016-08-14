module HttpX exposing (..)

import Error exposing (Error)
import Http
import Json.Decode exposing (Decoder)
import Task exposing (Task)

type alias Get a = Decoder a -> String -> Task Http.Error a

handleError : Http.Error -> Error
handleError error = case error of
  Http.Timeout -> Error.FailureToConnect
  Http.NetworkError -> Error.FailureToConnect
  Http.UnexpectedPayload payload -> Error.UnexpectedResponse payload
  Http.BadResponse status message -> Error.UnexpectedResponse (toString status ++ " " ++ message)
