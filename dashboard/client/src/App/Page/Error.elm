module App.Page.Error exposing (html)

import App.Page.Html exposing (role)
import Html exposing (..)
import Html.Attributes exposing (class)

import App.Error exposing (..)

html : Error -> List (Html a)
html error =
  let (message, log) =
    case error of
      UnknownError log -> ("There was an unknown error. Please try again later.", Just log)
      MissingPage name -> ("The " ++ name ++ " page is missing. Perhaps it hasn't been built yet?", Nothing)
      FailureToConnect -> ("We could not connect to the server. This may be a problem with your Internet connection, or may be on our end. Please try again later.", Nothing)
      UnexpectedResponse log -> ("It appears that the server has provided us with invalid data. We're not sure why, but it's probably worth checking that you're not behind a proxy, and if all else fails, trying again later.", Just log)
  in
    [div [role "alert", class "alert alert-danger"] [text message]] ++ debugging log

debugging : Maybe String -> List (Html a)
debugging log = maybeToList (Maybe.map (\value -> div [class "debug-log"] [text value]) log)

maybeToList : Maybe a -> List a
maybeToList maybe =
  case maybe of
    Nothing -> []
    Just value -> [value]
