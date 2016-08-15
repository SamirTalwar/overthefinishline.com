module Page.Error exposing (html)

import Error exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class)
import Page.Attributes exposing (role)

html : Error -> List (Html a)
html error =
  let message =
    case error of
      FailureToConnect -> "We could not connect to the server. This may be a problem with your Internet connection, or may be on our end. Please try again later."
      UnexpectedResponse _ -> "It appears that the server has provided us with invalid data. We're not sure why, but it's probably worth checking that you're not behind a proxy, and if all else fails, trying again later."
  in
    [
      div [role "alert", class "alert alert-danger"] [text message]
    ]
