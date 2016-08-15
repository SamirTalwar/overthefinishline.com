module Url exposing (
    Url,
    parse,
    toString,

    addQuery,

    decoder,

    href,
    src
  )

import Erl
import Html
import Html.Attributes
import Json.Decode exposing (..)

type alias Url = Erl.Url

parse : String -> Url
parse = Erl.parse

toString : Url -> String
toString = Erl.toString

addQuery : String -> String -> Url -> Url
addQuery = Erl.addQuery

decoder : Decoder Url
decoder = object1 Erl.parse string

href : Url -> Html.Attribute a
href = Erl.toString >> Html.Attributes.href

src : Url -> Html.Attribute a
src = Erl.toString >> Html.Attributes.src
