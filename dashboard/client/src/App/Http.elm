module App.Http exposing (
    Send,
    Response (..),
    Failures,
    Failure (..),
    get,
    get',
    decoder
  )

import Http
import Json.Decode exposing (..)
import Task exposing (Task)
import Url exposing (Url)

import App.Error exposing (..)
import App.Location as Location exposing (Location)

type Response a =
    UnauthenticatedResponse
  | Response Failures a

type alias Failures = List Failure
type Failure =
    RequestFailure Url String

type alias Send = Http.Request -> Task Http.RawError Http.Response

get : Location -> Decoder a -> Task Error (Response a)
get location underlyingDecoder = get' (Http.send Http.defaultSettings) location underlyingDecoder

get' : Send -> Location -> Decoder a -> Task Error (Response a)
get' send location underlyingDecoder =
  let prependPath segment url = { url | path = segment :: url.path }
      url = Url.toString (prependPath "api" (Location.url location))
      request = {
        verb = "GET",
        headers = [("Accept", "application/json")],
        url = url,
        body = Http.empty
      }
  in fromJson (decoder underlyingDecoder) (send request)

decoder : Decoder a -> Decoder (Response a)
decoder wrapped =
  ("state" := string) `andThen` \state ->
    case state of
      "Authenticated" ->
        object2 Response failuresDecoder wrapped
      "Unauthenticated" ->
        succeed UnauthenticatedResponse
      _ ->
          fail (state ++ " is not a recognized state")

failuresDecoder : Decoder Failures
failuresDecoder = map (Maybe.withDefault []) (maybe ("failures" := (list failureDecoder)))

failureDecoder : Decoder Failure
failureDecoder =
  ("tag" := string) `andThen` \tag ->
    case tag of
      "RequestFailure" ->
        object2 RequestFailure
          ("url" := Url.decoder)
          ("message" := string)
      _ ->
          fail (tag ++ " is not a recognized failure tag")

fromJson : Decoder (Response a) -> Task Http.RawError Http.Response -> Task Error (Response a)
fromJson decoder response =
  let
    decode str =
      case decodeString decoder str of
        Ok value -> Task.succeed value
        Err error -> Task.fail (UnexpectedResponse error)
  in
    Task.mapError (always FailureToConnect) response
      `Task.andThen` handleResponse decode

handleResponse : (String -> Task Error (Response a)) -> Http.Response -> Task Error (Response a)
handleResponse handle response =
  if 200 <= response.status && response.status < 300 then
    case response.value of
      Http.Text str ->
        handle str
      _ ->
        Task.fail (UnexpectedResponse "Response body is a blob, expecting a string.")
  else if response.status == 401 then
    Task.succeed UnauthenticatedResponse
  else
    Task.fail (UnexpectedResponse (toString response.status ++ " " ++ response.statusText))
