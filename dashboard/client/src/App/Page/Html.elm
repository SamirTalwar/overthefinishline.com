module App.Page.Html exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, property)
import Html.Events exposing (defaultOptions, onWithOptions)
import Json.Encode as Json
import Json.Decode
import String
import Url exposing (Url)

link : (Url -> message) -> Url -> List (Attribute message) -> List (Html message) -> Html message
link navigateTo url attributes = Html.a ([href url, onClick (navigateTo url)] ++ attributes)

href : Url -> Attribute a
href = Url.toString >> Html.Attributes.href

emptyHref : Attribute a
emptyHref = Html.Attributes.href "#"

src : Url -> Attribute a
src = Url.toString >> Html.Attributes.src

role : String -> Attribute a
role value = property "role" (Json.string value)

ariaExpanded : Bool -> Attribute a
ariaExpanded value = attribute "aria-expanded" (String.toLower (toString value))

ariaHaspopup : Bool -> Attribute a
ariaHaspopup value = attribute "aria-haspopup" (String.toLower (toString value))

onClick : message -> Attribute message
onClick = onWithOptions "click" { defaultOptions | preventDefault = True } << Json.Decode.succeed
