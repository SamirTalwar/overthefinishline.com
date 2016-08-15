module Page.Attributes exposing (..)

import Html
import Html.Attributes exposing (property)
import Json.Encode as Json

role : String -> Html.Attribute a
role value = property "role" (Json.string value)
