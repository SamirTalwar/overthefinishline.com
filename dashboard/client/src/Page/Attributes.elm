module Page.Attributes exposing (..)

import Html
import Html.Attributes exposing (attribute, property)
import Json.Encode as Json
import String
import Url exposing (Url)

href : Url -> Html.Attribute a
href = Url.toString >> Html.Attributes.href

emptyHref : Html.Attribute a
emptyHref = Html.Attributes.href "#"

src : Url -> Html.Attribute a
src = Url.toString >> Html.Attributes.src

role : String -> Html.Attribute a
role value = property "role" (Json.string value)

ariaExpanded : Bool -> Html.Attribute a
ariaExpanded value = attribute "aria-expanded" (String.toLower (toString value))

ariaHaspopup : Bool -> Html.Attribute a
ariaHaspopup value = attribute "aria-haspopup" (String.toLower (toString value))
