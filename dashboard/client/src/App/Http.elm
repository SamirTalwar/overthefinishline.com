module App.Http exposing (
    Response (..),
    Get,
    get,
    stubGet
  )

import Http
import Json.Decode exposing (..)
import Task exposing (Task)

import App.Error exposing (..)

type Response a =
    UnauthenticatedResponse
  | Response a

type alias Get a = Decoder a -> String -> Task Error (Response a)

get : Get a
get decoder url = Http.get (authenticationDecoder decoder) url |> Task.mapError handleError

stubGet : String -> String -> Get a
stubGet stubPath stubResponse decoder url =
  if url == stubPath then
    Task.fromResult (decodeString (authenticationDecoder decoder) stubResponse)
      |> Task.mapError UnexpectedResponse
  else
    Task.fail (UnexpectedResponse "Not Found")

authenticationDecoder : Decoder a -> Decoder (Response a)
authenticationDecoder wrapped =
  ("state" := string) `andThen` \state ->
    case state of
      "Authenticated" ->
        object1 Response wrapped
      "Unauthenticated" ->
        succeed UnauthenticatedResponse
      _ ->
          fail (state ++ " is not a recognized state")

handleError : Http.Error -> Error
handleError error = case error of
  Http.Timeout -> FailureToConnect
  Http.NetworkError -> FailureToConnect
  Http.UnexpectedPayload payload -> UnexpectedResponse payload
  Http.BadResponse status message -> UnexpectedResponse (toString status ++ " " ++ message)
