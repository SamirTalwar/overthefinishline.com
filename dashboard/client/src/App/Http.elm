module App.Http exposing (
    Response (..),
    get,
    decoder
  )

import Http
import Json.Decode exposing (..)
import Task exposing (Task)
import Url

import App.Error exposing (..)
import App.Location as Location exposing (Location)

type Response a =
    UnauthenticatedResponse
  | Response a

get : Location -> Decoder a -> Task Error (Response a)
get location underlyingDecoder =
  let prependPath segment url = { url | path = segment :: url.path }
      url = Url.toString (prependPath "api" (Location.url location))
      request = {
        verb = "GET",
        headers = [("Accept", "application/json")],
        url = url,
        body = Http.empty
      }
  in Http.fromJson (decoder underlyingDecoder) (Http.send Http.defaultSettings request)
      |> Task.mapError handleError

decoder : Decoder a -> Decoder (Response a)
decoder wrapped =
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
