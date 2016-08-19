module App.Page.Html exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes exposing (attribute, property)
import Html.Events exposing (defaultOptions, onWithOptions)
import Json.Encode as Json
import Json.Decode
import String
import Url exposing (Url)

import App.Location as Location exposing (Location)

link : (Location -> message) -> Location -> List (Attribute message) -> List (Html message) -> Html message
link navigateTo location attributes =
  Html.a ([href location, onClick (navigateTo location)] ++ attributes)

formFor : (Location -> message) -> Location -> List (Attribute message) -> List (Html message) -> Html message
formFor navigateTo location attributes =
  Html.form ([Attributes.method "post", action location] ++ attributes)

href : Location -> Attribute a
href = Location.url >> Url.toString >> Attributes.href

action : Location -> Attribute a
action = Location.url >> Url.toString >> Attributes.action

emptyHref : Attribute a
emptyHref = Attributes.href "#"

src : Url -> Attribute a
src = Url.toString >> Attributes.src

role : String -> Attribute a
role value = property "role" (Json.string value)

ariaExpanded : Bool -> Attribute a
ariaExpanded value = attribute "aria-expanded" (String.toLower (toString value))

ariaHaspopup : Bool -> Attribute a
ariaHaspopup value = attribute "aria-haspopup" (String.toLower (toString value))

onClick : message -> Attribute message
onClick = onWithOptions "click" { defaultOptions | preventDefault = True } << Json.Decode.succeed
