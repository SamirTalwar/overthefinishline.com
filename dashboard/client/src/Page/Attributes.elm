module Page.Attributes exposing (..)

import Html
import Html.Attributes exposing (attribute, property)
import Json.Encode as Json
import String

role : String -> Html.Attribute a
role value = property "role" (Json.string value)

ariaExpanded : Bool -> Html.Attribute a
ariaExpanded value = attribute "aria-expanded" (String.toLower (toString value))

ariaHaspopup : Bool -> Html.Attribute a
ariaHaspopup value = attribute "aria-haspopup" (String.toLower (toString value))
