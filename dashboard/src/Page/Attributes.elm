module Page.Attributes where

import Html.Attributes exposing (property)
import Json.Encode as Json

role value = property "role" (Json.string value)
