module Url exposing (
    Url,
    empty,
    withPath,
    parse,
    toString,

    addQuery,

    decoder
  )

import Erl
import Json.Decode exposing (..)

type alias Url = Erl.Url

empty : Url
empty = Erl.new

withPath : List String -> Url
withPath path = let url = parse "/" in { url | path = path, hasTrailingSlash = False }

parse : String -> Url
parse = Erl.parse

toString : Url -> String
toString url =
  let urlString = Erl.toString url in if urlString == "" then "/" else urlString

addQuery : String -> String -> Url -> Url
addQuery = Erl.addQuery

decoder : Decoder Url
decoder = map Erl.parse string
