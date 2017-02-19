module App.Http exposing (get)

import Http
import Json.Decode exposing (..)
import Url exposing (Url)

import App.Error exposing (..)
import App.Location as Location exposing (Location)
import App.Model exposing (Failure (..), Failures, Message (ErrorMessage), Response (..))

get : Location -> Decoder a -> (Response a -> Message) -> Cmd Message
get location underlyingDecoder messageConstructor =
  let prependPath segment url = { url | path = segment :: url.path }
      url = Url.toString (prependPath "api" (Location.url location))
      httpRequest = Http.request {
        method = "GET",
        headers = [Http.header "Accept" "application/json"],
        url = url,
        body = Http.emptyBody,
        expect = Http.expectJson (decoder underlyingDecoder),
        timeout = Nothing,
        withCredentials = False
      }
  in Http.send (processResponse messageConstructor) httpRequest

decoder : Decoder a -> Decoder (Response a)
decoder wrapped =
  (field "state" string) |> andThen (\state ->
    case state of
      "Authenticated" ->
        map2 Response failuresDecoder wrapped
      "Unauthenticated" ->
        succeed UnauthenticatedResponse
      _ ->
        fail (state ++ " is not a recognized state"))

failuresDecoder : Decoder Failures
failuresDecoder = map (Maybe.withDefault []) (maybe (field "failures" (list failureDecoder)))

failureDecoder : Decoder Failure
failureDecoder =
  (field "tag" string) |> andThen (\tag ->
    case tag of
      "RequestFailure" ->
        map2 RequestFailure
          (field "url" Url.decoder)
          (field "message" string)
      _ ->
        fail (tag ++ " is not a recognized failure tag"))

processResponse : (Response a -> Message) -> Result Http.Error (Response a) -> Message
processResponse messageConstructor response =
  case response of
    Ok validResponse -> messageConstructor validResponse
    Err error -> ErrorMessage (appError error)

appError : Http.Error -> Error
appError error =
  case error of
    Http.BadUrl url -> UnknownError ("Bad URL: " ++ url)
    Http.Timeout -> FailureToConnect
    Http.NetworkError -> FailureToConnect
    Http.BadStatus response ->
      UnexpectedResponse ("Status: " ++ toString response.status.code ++ " " ++ response.status.message
                       ++ "\nResponse: " ++ toString response)
    Http.BadPayload message response ->
      UnexpectedResponse ("Error: " ++ message
                       ++ "\nResponse: " ++ toString response)
